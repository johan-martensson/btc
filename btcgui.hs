{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.String      (CString, withCString, peekCString)
import Foreign.C.Types       (CInt(..))
import Foreign.Ptr            (Ptr, nullPtr)
import Foreign.Marshal.Array  (allocaArray)
import Data.Char              (isDigit, isSpace)
import Data.List              (groupBy, isPrefixOf)
import Data.Time              (Day, UTCTime(..), parseTimeM, defaultTimeLocale,
                               formatTime, utctDay, getCurrentTime, addDays)
import Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import System.Environment     (getArgs)
import Text.Printf            (printf)

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

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

openBrowser :: String -> IO ()
openBrowser url = do
  _ <- readCommand ("open " ++ url)
  return ()

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type DailyEntry = (Day, Double)

fetchCurrentPrice :: IO (Maybe (String, String, String))
fetchCurrentPrice = do
  response <- readCommand
    "curl -s 'https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies=usd,eur,gbp'"
  return $ do
    usd <- extractJsonValue "usd" response
    eur <- extractJsonValue "eur" response
    gbp <- extractJsonValue "gbp" response
    Just (usd, eur, gbp)

fetchHistoricalPrices :: Day -> Day -> IO (Either String [DailyEntry])
fetchHistoricalPrices fromDay toDay = do
  let fromEpoch = dayToEpoch fromDay
      toEpoch   = dayToEpoch toDay + 86400
      url = "curl -s 'https://api.coingecko.com/api/v3/coins/bitcoin/market_chart/range"
            ++ "?vs_currency=usd&from=" ++ show fromEpoch
            ++ "&to=" ++ show toEpoch ++ "'"
  response <- readCommand url
  case parsePriceArray response of
    Nothing -> return $ Left ("Could not parse API response: " ++ take 200 response)
    Just [] -> return $ Left "No price data returned for this range."
    Just entries -> return $ Right (downsampleDaily entries)

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

-------------------------------------------------------------------------------
-- Date helpers
-------------------------------------------------------------------------------

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

dayToEpoch :: Day -> Integer
dayToEpoch d = round $ utcTimeToPOSIXSeconds (UTCTime d 0)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"

downsampleDaily :: [(Double, Double)] -> [DailyEntry]
downsampleDaily entries =
  let withDays = map (\(ts, p) -> (utctDay (msToUTC ts), p)) entries
      grouped  = groupBy (\a b -> fst a == fst b) withDays
  in  map (\grp -> (fst (head grp), snd (head grp))) grouped

msToUTC :: Double -> UTCTime
msToUTC ms = posixSecondsToUTCTime (realToFrac (ms / 1000))

-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

showPrice :: Double -> String
showPrice p = printf "%.2f" p

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

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  now  <- getCurrentTime
  let today    = utctDay now
      defFrom  = showDay (addDays (-90) today)
      defTo    = showDay today

  (fromStr, toStr) <- case args of
    [f, t] -> return (f, t)
    _      -> return (defFrom, defTo)

  putStrLn $ "Fetching Bitcoin data for " ++ fromStr ++ " to " ++ toStr ++ " ..."

  -- Fetch current price
  currentResult <- fetchCurrentPrice
  let (curUsd, curEur, curGbp) = case currentResult of
        Just x  -> x
        Nothing -> ("--", "--", "--")

  -- Fetch historical prices
  case (parseDay fromStr, parseDay toStr) of
    (Just fromDay, Just toDay) | fromDay < toDay -> do
      histResult <- fetchHistoricalPrices fromDay toDay
      case histResult of
        Left err -> do
          putStrLn $ "Error: " ++ err
        Right entries -> do
          let htmlPath = "/tmp/btcprice.html"
          writeFile htmlPath (generateHtml curUsd curEur curGbp entries)
          putStrLn $ "Opening " ++ htmlPath ++ " in browser..."
          openBrowser htmlPath
    _ -> putStrLn "Error: invalid or reversed date range. Use: btcgui YYYY-MM-DD YYYY-MM-DD"

-------------------------------------------------------------------------------
-- HTML generation
-------------------------------------------------------------------------------

generateHtml :: String -> String -> String -> [DailyEntry] -> String
generateHtml curUsd curEur curGbp entries =
  let prices    = map snd entries
      minP      = minimum prices
      maxP      = maximum prices
      avgP      = sum prices / fromIntegral (length prices)
      firstP    = snd (head entries)
      lastP     = snd (last entries)
      changePct = (lastP - firstP) / firstP * 100 :: Double
      firstDay  = showDay (fst (head entries))
      lastDay   = showDay (fst (last entries))

      labelsJs  = "[" ++ joinWith "," (map (\(d,_) -> "\"" ++ showDay d ++ "\"") entries) ++ "]"
      pricesJs  = "[" ++ joinWith "," (map (\(_,p) -> printf "%.2f" p) entries) ++ "]"

      tableRows = concatMap makeRow (addChanges entries)
  in
  "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>"
  ++ "<meta name='viewport' content='width=device-width,initial-scale=1'>"
  ++ "<title>Bitcoin Price Tracker</title>"
  ++ "<script src='https://cdn.jsdelivr.net/npm/chart.js'></script>"
  ++ "<style>" ++ cssBlock ++ "</style></head><body>"
  ++ "<div class='container'>"
  ++ "<h1>\x20BF Bitcoin Price Tracker</h1>"

  -- Current price card
  ++ "<div class='section'>"
  ++ "<h2>Current Price</h2>"
  ++ "<div class='current-price'>"
  ++ priceChip "USD" "$" curUsd
  ++ priceChip "EUR" "\x20AC" curEur
  ++ priceChip "GBP" "\x00A3" curGbp
  ++ "</div></div>"

  -- Stats cards
  ++ "<div class='section'>"
  ++ "<h2>Period: " ++ firstDay ++ " &rarr; " ++ lastDay
  ++ "  <span class='day-count'>(" ++ show (length entries) ++ " days)</span></h2>"
  ++ "<div class='stats-grid'>"
  ++ statCard "High"    ("$" ++ formatNum (showPrice maxP)) ""
  ++ statCard "Low"     ("$" ++ formatNum (showPrice minP)) ""
  ++ statCard "Average" ("$" ++ formatNum (showPrice avgP)) ""
  ++ statCard "Change"  (printf "%+.2f%%" changePct)
              (if changePct >= 0 then "positive" else "negative")
  ++ "</div></div>"

  -- Chart
  ++ "<div class='section'>"
  ++ "<canvas id='priceChart'></canvas>"
  ++ "</div>"

  -- Table
  ++ "<div class='section'>"
  ++ "<h2>Daily Prices</h2>"
  ++ "<div class='table-scroll'>"
  ++ "<table class='price-table'>"
  ++ "<thead><tr><th>Date</th><th>Price (USD)</th><th>Change</th></tr></thead>"
  ++ "<tbody>" ++ tableRows ++ "</tbody></table></div></div>"

  ++ "</div>"

  -- Chart.js init
  ++ "<script>"
  ++ "const ctx=document.getElementById('priceChart').getContext('2d');"
  ++ "new Chart(ctx,{type:'line',data:{"
  ++ "labels:" ++ labelsJs ++ ","
  ++ "datasets:[{label:'BTC Price (USD)',"
  ++ "data:" ++ pricesJs ++ ","
  ++ "borderColor:'#f7931a',"
  ++ "backgroundColor:'rgba(247,147,26,0.08)',"
  ++ "borderWidth:2,fill:true,tension:0.2,pointRadius:0,pointHitRadius:10,"
  ++ "pointHoverRadius:5,pointHoverBackgroundColor:'#f7931a'}]},"
  ++ "options:{responsive:true,interaction:{mode:'index',intersect:false},"
  ++ "plugins:{legend:{labels:{color:'#ccc',font:{size:14}}},"
  ++ "tooltip:{backgroundColor:'#1e1e3a',titleColor:'#f7931a',bodyColor:'#e0e0e0',"
  ++ "borderColor:'#f7931a',borderWidth:1,padding:12,"
  ++ "callbacks:{label:function(c){return ' $'+Number(c.raw).toLocaleString(undefined,"
  ++ "{minimumFractionDigits:2,maximumFractionDigits:2})}}}},"
  ++ "scales:{x:{ticks:{color:'#888',maxTicksLimit:12,font:{size:11}},"
  ++ "grid:{color:'rgba(255,255,255,0.05)'}},"
  ++ "y:{ticks:{color:'#888',font:{size:11},"
  ++ "callback:function(v){return '$'+v.toLocaleString()}},"
  ++ "grid:{color:'rgba(255,255,255,0.05)'}}}}});"
  ++ "</script>"
  ++ "</body></html>"

priceChip :: String -> String -> String -> String
priceChip label symbol val =
     "<div class='price-chip'>"
  ++ "<span class='chip-label'>" ++ label ++ "</span>"
  ++ "<span class='chip-value'>" ++ symbol ++ formatNum val ++ "</span>"
  ++ "</div>"

statCard :: String -> String -> String -> String
statCard label value cls =
     "<div class='stat-card'>"
  ++ "<div class='stat-label'>" ++ label ++ "</div>"
  ++ "<div class='stat-value " ++ cls ++ "'>" ++ value ++ "</div>"
  ++ "</div>"

addChanges :: [DailyEntry] -> [(String, String, String, String)]
addChanges [] = []
addChanges (e:es) = (showDay (fst e), "$" ++ formatNum (showPrice (snd e)), "", "") : go (snd e) es
  where
    go _    []         = []
    go prev ((d,p):xs) =
      let pct = (p - prev) / prev * 100 :: Double
          cls = if pct >= 0 then "positive" else "negative"
      in  (showDay d, "$" ++ formatNum (showPrice p), printf "%+.2f%%" pct, cls) : go p xs

makeRow :: (String, String, String, String) -> String
makeRow (date, price, change, cls) =
     "<tr><td>" ++ date ++ "</td>"
  ++ "<td class='price-cell'>" ++ price ++ "</td>"
  ++ "<td class='" ++ cls ++ "'>" ++ change ++ "</td></tr>"

joinWith :: String -> [String] -> String
joinWith _   []     = ""
joinWith _   [x]    = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs

-------------------------------------------------------------------------------
-- CSS
-------------------------------------------------------------------------------

cssBlock :: String
cssBlock = concat
  [ "*{margin:0;padding:0;box-sizing:border-box}"
  , "body{background:#0f0f23;color:#e0e0e0;font-family:-apple-system,'Segoe UI',Roboto,'Helvetica Neue',sans-serif}"
  , ".container{max-width:1000px;margin:0 auto;padding:28px 20px}"
  , "h1{color:#f7931a;margin-bottom:28px;font-size:32px;font-weight:800;letter-spacing:-0.5px}"
  , "h2{color:#bbb;margin-bottom:14px;font-size:16px;font-weight:600}"
  , ".day-count{color:#666;font-weight:400}"
  , ".section{background:#16213e;border-radius:14px;padding:22px;margin-bottom:18px}"
  , ".current-price{display:flex;gap:20px;flex-wrap:wrap}"
  , ".price-chip{background:#0f3460;border-radius:10px;padding:14px 22px;display:flex;flex-direction:column;gap:2px;min-width:160px}"
  , ".chip-label{color:#888;font-size:12px;font-weight:500;text-transform:uppercase;letter-spacing:1px}"
  , ".chip-value{font-size:26px;font-weight:800;color:#fff}"
  , ".stats-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:14px}"
  , ".stat-card{background:#0f3460;border-radius:10px;padding:18px;text-align:center}"
  , ".stat-label{color:#888;font-size:11px;text-transform:uppercase;letter-spacing:1px;margin-bottom:6px}"
  , ".stat-value{font-size:22px;font-weight:800}"
  , ".positive{color:#2ecc71}"
  , ".negative{color:#e74c3c}"
  , "canvas{margin:4px 0}"
  , ".table-scroll{max-height:500px;overflow-y:auto}"
  , ".price-table{width:100%;border-collapse:collapse;font-size:13px}"
  , ".price-table thead{position:sticky;top:0;background:#16213e;z-index:1}"
  , ".price-table th{text-align:left;padding:10px 14px;border-bottom:2px solid #333;color:#888;font-weight:600}"
  , ".price-table td{padding:7px 14px;border-bottom:1px solid #1a1a3e}"
  , ".price-table tr:hover{background:#0f3460}"
  , ".price-cell{font-variant-numeric:tabular-nums;font-weight:500}"
  , "::-webkit-scrollbar{width:8px}"
  , "::-webkit-scrollbar-track{background:#0f0f23;border-radius:4px}"
  , "::-webkit-scrollbar-thumb{background:#333;border-radius:4px}"
  ]
