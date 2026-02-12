{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.String    (CString, withCString, peekCString)
import Foreign.C.Types     (CInt(..))
import Foreign.Ptr          (Ptr, nullPtr)
import Foreign.Marshal.Array (allocaArray)
import Data.Char            (isDigit, isSpace)
import Data.List            (groupBy, isPrefixOf)
import Data.Time            (Day, UTCTime(..), parseTimeM, defaultTimeLocale,
                             formatTime, utctDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import System.Environment   (getArgs)
import Text.Printf          (printf)

-- FFI bindings to libc popen/pclose/fgets
data CFile
foreign import ccall "popen"  c_popen  :: CString -> CString -> IO (Ptr CFile)
foreign import ccall "pclose" c_pclose :: Ptr CFile -> IO CInt
foreign import ccall "fgets"  c_fgets  :: CString -> CInt -> Ptr CFile -> IO CString

readCommand :: String -> IO String
readCommand cmd =
  withCString cmd $ \cCmd ->
  withCString "r" $ \cMode -> do
    fp <- c_popen cCmd cMode
    if fp == nullPtr
      then return ""
      else do
        output <- readAll fp
        _ <- c_pclose fp
        return output
  where
    bufSize = 4096
    readAll fp = allocaArray bufSize $ \buf -> go fp buf ""
    go fp buf acc = do
      result <- c_fgets buf (fromIntegral bufSize) fp
      if result == nullPtr
        then return acc
        else do
          chunk <- peekCString buf
          go fp buf (acc ++ chunk)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> currentPrice
    [from, to]   -> historicalPrices from to
    _            -> putStrLn usage

usage :: String
usage = unlines
  [ "Usage:"
  , "  btcprice                          Show current Bitcoin price"
  , "  btcprice FROM TO                  Daily price stats for date range"
  , ""
  , "  Dates must be YYYY-MM-DD, e.g.:   btcprice 2025-08-01 2025-10-15"
  , "  CoinGecko limited to last 365 days; falls back to Binance for longer ranges."
  ]

-------------------------------------------------------------------------------
-- Current price (CoinGecko with Binance fallback)
-------------------------------------------------------------------------------

currentPrice :: IO ()
currentPrice = do
  result <- fetchCurrentCoinGecko
  case result of
    Just (usd, eur, gbp, vol) -> printCurrentPrice "CoinGecko" usd eur gbp vol
    Nothing -> do
      putStrLn "CoinGecko unavailable, trying Binance..."
      result2 <- fetchCurrentBinance
      case result2 of
        Just (usd, eur, gbp, vol) -> printCurrentPrice "Binance" usd eur gbp vol
        Nothing -> putStrLn "Error: could not fetch price from any source"

printCurrentPrice :: String -> String -> String -> String -> String -> IO ()
printCurrentPrice src usd eur gbp vol = do
  putStrLn $ "=== Current Bitcoin Price (" ++ src ++ ") ==="
  putStrLn $ "  USD: $" ++ formatNum usd
  putStrLn $ "  EUR: \x20AC" ++ formatNum eur
  putStrLn $ "  GBP: \x00A3" ++ formatNum gbp
  putStrLn $ "  24h Volume: $" ++ formatNum (formatVolumeStr vol)

fetchCurrentCoinGecko :: IO (Maybe (String, String, String, String))
fetchCurrentCoinGecko = do
  response <- readCommand
    "curl -s --max-time 5 'https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies=usd,eur,gbp&include_24hr_vol=true'"
  return $ do
    usd <- extractJsonValue "usd" response
    eur <- extractJsonValue "eur" response
    gbp <- extractJsonValue "gbp" response
    vol <- extractJsonValue "usd_24h_vol" response
    Just (usd, eur, gbp, vol)

fetchCurrentBinance :: IO (Maybe (String, String, String, String))
fetchCurrentBinance = do
  rUsd <- readCommand "curl -s --max-time 5 'https://api.binance.com/api/v3/ticker/24hr?symbol=BTCUSDT'"
  rEur <- readCommand "curl -s --max-time 5 'https://api.binance.com/api/v3/ticker/price?symbol=BTCEUR'"
  rGbp <- readCommand "curl -s --max-time 5 'https://api.binance.com/api/v3/ticker/price?symbol=BTCGBP'"
  return $ do
    usd <- extractQuotedValue "lastPrice" rUsd
    eur <- extractQuotedValue "price" rEur
    gbp <- extractQuotedValue "price" rGbp
    vol <- extractQuotedValue "quoteVolume" rUsd
    Just (trimTrailingZeros usd, trimTrailingZeros eur, trimTrailingZeros gbp, trimTrailingZeros vol)

-------------------------------------------------------------------------------
-- Historical daily prices
-------------------------------------------------------------------------------

historicalPrices :: String -> String -> IO ()
historicalPrices fromStr toStr =
  case (parseDay fromStr, parseDay toStr) of
    (Just fromDay, Just toDay)
      | fromDay >= toDay -> putStrLn "Error: FROM date must be before TO date"
      | otherwise -> do
          result <- fetchHistoricalCoinGecko fromDay toDay
          case result of
            Just entries -> printDailyTable entries
            Nothing -> do
              putStrLn "CoinGecko unavailable, trying Binance..."
              result2 <- fetchHistoricalBinance fromDay toDay
              case result2 of
                Just entries -> printDailyTable entries
                Nothing -> putStrLn "Error: could not fetch historical data from any source"
    _ -> putStrLn $ "Error: invalid date format. Use YYYY-MM-DD.\n\n" ++ usage

fetchHistoricalCoinGecko :: Day -> Day -> IO (Maybe [DailyEntry])
fetchHistoricalCoinGecko fromDay toDay = do
  let fromEpoch = dayToEpoch fromDay
      toEpoch   = dayToEpoch toDay + 86400
      url = "curl -s --max-time 10 'https://api.coingecko.com/api/v3/coins/bitcoin/market_chart/range"
            ++ "?vs_currency=usd&from=" ++ show fromEpoch
            ++ "&to=" ++ show toEpoch ++ "'"
  response <- readCommand url
  case parsePriceArray response of
    Just entries@(_:_) ->
      let volumes = parseVolumeArray response
          daily   = downsampleDaily entries volumes
      in  return $ Just daily
    _ -> return Nothing

-- Binance klines: [[openTime,"open","high","low","close",...], ...]
-- We use close price (index 4). Paginates in 1000-day chunks.
fetchHistoricalBinance :: Day -> Day -> IO (Maybe [DailyEntry])
fetchHistoricalBinance fromDay toDay = do
  entries <- fetchBinanceChunks (dayToEpoch fromDay * 1000) ((dayToEpoch toDay + 86400) * 1000)
  case entries of
    [] -> return Nothing
    _  -> return $ Just entries

fetchBinanceChunks :: Integer -> Integer -> IO [DailyEntry]
fetchBinanceChunks fromMs toMs
  | fromMs >= toMs = return []
  | otherwise = do
      let url = "curl -s --max-time 10 'https://api.binance.com/api/v3/klines"
                ++ "?symbol=BTCUSDT&interval=1d&startTime=" ++ show fromMs
                ++ "&endTime=" ++ show toMs ++ "&limit=1000'"
      response <- readCommand url
      let entries = parseBinanceKlines response
      case entries of
        [] -> return []
        _  -> do
          let (lastDay, _, _) = last entries
              nextFromMs = (dayToEpoch lastDay + 86400) * 1000
          if nextFromMs >= toMs || length entries < 1000
            then return entries
            else do
              rest <- fetchBinanceChunks nextFromMs toMs
              return (entries ++ rest)

-- Parse Binance kline array: [[ts,"o","h","l","c","vol",closeTime,"quoteVol",...], ...]
-- Extracts close price (index 4) and quoteAssetVolume (index 7)
parseBinanceKlines :: String -> [DailyEntry]
parseBinanceKlines s = case dropWhile (/= '[') s of
  '[':'[':rest -> parseKline rest
  _            -> []
  where
    parseKline r =
      case reads r :: [(Double, String)] of
        [(ts, r1)] ->
          case skipQuoted (dropComma r1) of            -- skip open (1)
            Just r2 -> case skipQuoted (dropComma r2) of    -- skip high (2)
              Just r3 -> case skipQuoted (dropComma r3) of    -- skip low (3)
                Just r4 -> case readQuoted (dropComma r4) of    -- read close (4)
                  Just (closeStr, r5) ->
                    case readQuoted (dropComma r5) of               -- skip volume (5)
                      Just (_, r6) -> case skipNumber (dropComma r6) of  -- skip closeTime (6)
                        Just r7 -> case readQuoted (dropComma r7) of     -- read quoteAssetVolume (7)
                          Just (volStr, _) ->
                            case (readMaybe' closeStr, readMaybe' volStr) of
                              (Just price, Just vol) ->
                                let day = utctDay (msToUTC ts)
                                in  (day, price, vol) : parseKline (skipToNext r)
                              _ -> parseKline (skipToNext r)
                          _ -> parseKline (skipToNext r)
                        _ -> parseKline (skipToNext r)
                      _ -> parseKline (skipToNext r)
                  _ -> parseKline (skipToNext r)
                _ -> parseKline (skipToNext r)
              _ -> parseKline (skipToNext r)
            _ -> parseKline (skipToNext r)
        _ -> []
    dropComma = drop 1 . dropWhile (/= ',')
    skipQuoted str = case dropWhile (/= '"') str of
      '"':rest -> Just (drop 1 (dropWhile (/= '"') rest))
      _        -> Nothing
    readQuoted str = case dropWhile (/= '"') str of
      '"':rest -> let (val, rest2) = span (/= '"') rest
                  in  Just (val, drop 1 rest2)
      _        -> Nothing
    skipNumber str =
      let trimmed = dropWhile isSpace str
      in  case reads trimmed :: [(Double, String)] of
            [(_,rest)] -> Just rest
            _          -> Nothing
    skipToNext str = case dropWhile (/= '[') str of
      '[':rest -> rest
      _        -> ""

type DailyEntry = (Day, Double, Double)  -- (date, price, volume)

printDailyTable :: [DailyEntry] -> IO ()
printDailyTable [] = putStrLn "No data."
printDailyTable entries = do
  let prices    = map (\(_,p,_) -> p) entries
      volumes   = map (\(_,_,v) -> v) entries
      minP      = minimum prices
      maxP      = maximum prices
      avgP      = sum prices / fromIntegral (length prices)
      totalVol  = sum volumes
      avgVol    = totalVol / fromIntegral (length volumes)
      (_,firstP,_) = head entries
      (_,lastP,_)  = last entries
      changePct = (lastP - firstP) / firstP * 100
      (firstDay,_,_) = head entries
      (lastDay,_,_)  = last entries

  putStrLn $ "=== Bitcoin Daily Prices: "
           ++ showDay firstDay ++ " to " ++ showDay lastDay ++ " ==="
  putStrLn ""
  putStrLn $ padR 12 "Date" ++ padR 16 "Price (USD)" ++ padR 22 "Volume (USD)" ++ "Change"
  putStrLn $ replicate 60 '-'
  mapM_ printRow (withChanges entries)
  putStrLn $ replicate 60 '-'
  putStrLn ""
  putStrLn "=== Summary ==="
  printf   "  High:       $%s\n" (formatNum (showPrice maxP))
  printf   "  Low:        $%s\n" (formatNum (showPrice minP))
  printf   "  Average:    $%s\n" (formatNum (showPrice avgP))
  printf   "  Change:     %+.2f%%\n" changePct
  printf   "  Total Vol:  $%s\n" (formatNum (showVolume totalVol))
  printf   "  Avg Vol:    $%s\n" (formatNum (showVolume avgVol))

withChanges :: [DailyEntry] -> [(String, Double, Double, String)]
withChanges [] = []
withChanges ((d,p,v):es) = (showDay d, p, v, "") : go p es
  where
    go _    []              = []
    go prev ((d',p',v'):xs) =
      let pct = (p' - prev) / prev * 100 :: Double
          s   = printf "%+.2f%%" pct
      in  (showDay d', p', v', s) : go p' xs

printRow :: (String, Double, Double, String) -> IO ()
printRow (d, p, v, ch) =
  putStrLn $ padR 12 d
          ++ padR 16 ("$" ++ formatNum (showPrice p))
          ++ padR 22 ("$" ++ formatNum (showVolume v))
          ++ ch

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

-------------------------------------------------------------------------------
-- JSON parsing
-------------------------------------------------------------------------------

extractJsonValue :: String -> String -> Maybe String
extractJsonValue _   [] = Nothing
extractJsonValue key json
  | needle `isPrefixOf` json =
      Just (takeWhile (\c -> isDigit c || c == '.') (drop (length needle) json))
  | otherwise = extractJsonValue key (drop 1 json)
  where needle = "\"" ++ key ++ "\":"

parsePriceArray :: String -> Maybe [(Double, Double)]
parsePriceArray json = do
  rest <- scanPrefix "\"prices\":[" json
  Just (parsePairs rest)

parseVolumeArray :: String -> [(Double, Double)]
parseVolumeArray json =
  case scanPrefix "\"total_volumes\":[" json of
    Just rest -> parsePairs rest
    Nothing   -> []

scanPrefix :: String -> String -> Maybe String
scanPrefix [] ys         = Just ys
scanPrefix _  []         = Nothing
scanPrefix (x:xs) (y:ys)
  | x == y    = scanPrefix xs ys
  | otherwise = scanPrefix (x:xs) (drop 1 (y:ys))

parsePairs :: String -> [(Double, Double)]
parsePairs s = case dropWhile (\c -> c /= '[' && c /= ']') s of
  ('[':rest) ->
    case span (/= ',') rest of
      (tsStr, ',':rest2) ->
        case span (/= ']') rest2 of
          (priceStr, ']':rest3) ->
            case (readMaybe' tsStr, readMaybe' priceStr) of
              (Just ts, Just price) -> (ts, price) : parsePairs rest3
              _                     -> parsePairs rest3
          _ -> []
      _ -> []
  (']':']':_) -> []
  (']':_)     -> []
  _           -> []

readMaybe' :: String -> Maybe Double
readMaybe' s = case reads (dropWhile isSpace s) of
  [(v, _)] -> Just v
  _        -> Nothing

-- Extract "key":"value" (quoted value, for Binance)
extractQuotedValue :: String -> String -> Maybe String
extractQuotedValue _   [] = Nothing
extractQuotedValue key json
  | needle `isPrefixOf` json =
      let rest = drop (length needle) json
      in  Just (takeWhile (/= '"') rest)
  | otherwise = extractQuotedValue key (drop 1 json)
  where needle = "\"" ++ key ++ "\":\""

-- Remove unnecessary trailing zeros from price strings like "67125.35000000"
trimTrailingZeros :: String -> String
trimTrailingZeros s
  | '.' `elem` s = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse $ s
  | otherwise     = s

-------------------------------------------------------------------------------
-- Date / time helpers
-------------------------------------------------------------------------------

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

dayToEpoch :: Day -> Integer
dayToEpoch d = round $ utcTimeToPOSIXSeconds (UTCTime d 0)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"

downsampleDaily :: [(Double, Double)] -> [(Double, Double)] -> [DailyEntry]
downsampleDaily priceEntries volumeEntries =
  let priceDays = map (\(ts, p) -> (utctDay (msToUTC ts), p)) priceEntries
      volMap    = map (\(ts, v) -> (utctDay (msToUTC ts), v)) volumeEntries
      grouped   = groupBy (\a b -> fst a == fst b) priceDays
      dailyP    = map (\grp -> (fst (head grp), snd (head grp))) grouped
      volGrouped = groupBy (\a b -> fst a == fst b) volMap
      dailyV    = map (\grp -> (fst (head grp), snd (head grp))) volGrouped
      volLookup = dailyV
  in  map (\(d, p) -> (d, p, lookupVol d volLookup)) dailyP

lookupVol :: Day -> [(Day, Double)] -> Double
lookupVol _ [] = 0
lookupVol d ((d',v):rest)
  | d == d'   = v
  | otherwise = lookupVol d rest

msToUTC :: Double -> UTCTime
msToUTC ms = posixSecondsToUTCTime (realToFrac (ms / 1000))

-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

showPrice :: Double -> String
showPrice p = printf "%.2f" p

showVolume :: Double -> String
showVolume v = printf "%.0f" v

formatVolumeStr :: String -> String
formatVolumeStr s =
  let (whole, _frac) = break (== '.') s
  in  whole

formatNum :: String -> String
formatNum s =
  let (whole, frac) = break (== '.') s
      grouped = reverse . insertCommas . reverse $ whole
  in  grouped ++ frac
  where
    insertCommas [] = []
    insertCommas xs
      | length xs <= 3 = xs
      | otherwise      = take 3 xs ++ "," ++ insertCommas (drop 3 xs)
