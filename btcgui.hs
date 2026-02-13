{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.String      (CString, withCString, peekCString)
import Foreign.C.Types       (CInt(..))
import Foreign.Ptr            (Ptr, nullPtr)
import Foreign.Marshal.Array  (allocaArray)
import Data.Char              (isDigit, isSpace)
import Data.List              (groupBy, isPrefixOf)
import Data.Time              (Day, UTCTime(..), parseTimeM, defaultTimeLocale,
                               formatTime, utctDay, getCurrentTime, addDays, diffDays)
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

type PriceEntry = (String, Double, Double)  -- (label, price, volume)

fetchCurrentPrice :: IO (Maybe (String, String, String, String))
fetchCurrentPrice = do
  result <- fetchCurrentCoinGecko
  case result of
    Just x  -> return (Just x)
    Nothing -> fetchCurrentBinance

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
    Just (trimZeros usd, trimZeros eur, trimZeros gbp, trimZeros vol)

fetchHistoricalPrices :: Bool -> Day -> Day -> IO (Either String [PriceEntry])
fetchHistoricalPrices isHourly fromDay toDay = do
  result <- fetchHistCoinGecko isHourly fromDay toDay
  case result of
    Just entries -> return $ Right entries
    Nothing -> do
      result2 <- fetchHistBinance isHourly fromDay toDay
      case result2 of
        Just entries -> return $ Right entries
        Nothing      -> return $ Left "Could not fetch data from any source"

fetchHistCoinGecko :: Bool -> Day -> Day -> IO (Maybe [PriceEntry])
fetchHistCoinGecko isHourly fromDay toDay = do
  let fromEpoch = dayToEpoch fromDay
      toEpoch   = dayToEpoch toDay + 86400
      url = "curl -s --max-time 10 'https://api.coingecko.com/api/v3/coins/bitcoin/market_chart/range"
            ++ "?vs_currency=usd&from=" ++ show fromEpoch
            ++ "&to=" ++ show toEpoch ++ "'"
  response <- readCommand url
  case parsePriceArray response of
    Just entries@(_:_) ->
      let volumes = parseVolumeArray response
          sampled = if isHourly
                    then downsampleHourly entries volumes
                    else downsampleDaily entries volumes
      in  return $ Just sampled
    _ -> return Nothing

fetchHistBinance :: Bool -> Day -> Day -> IO (Maybe [PriceEntry])
fetchHistBinance isHourly fromDay toDay = do
  entries <- fetchBinChunks isHourly (dayToEpoch fromDay * 1000) ((dayToEpoch toDay + 86400) * 1000)
  case entries of
    [] -> return Nothing
    _  -> return $ Just entries

fetchBinChunks :: Bool -> Integer -> Integer -> IO [PriceEntry]
fetchBinChunks isHourly fromMs toMs
  | fromMs >= toMs = return []
  | otherwise = do
      let interval = if isHourly then "1h" else "1d"
          url = "curl -s --max-time 10 'https://api.binance.com/api/v3/klines"
                ++ "?symbol=BTCUSDT&interval=" ++ interval ++ "&startTime=" ++ show fromMs
                ++ "&endTime=" ++ show toMs ++ "&limit=1000'"
      response <- readCommand url
      let entries = parseBinanceKlines isHourly response
      case entries of
        [] -> return []
        _  -> do
          let (lastLabel, _, _) = last entries
              step = if isHourly then 3600000 else 86400000
              nextFromMs = labelToMs lastLabel + step
          if nextFromMs >= toMs || length entries < 1000
            then return entries
            else do
              rest <- fetchBinChunks isHourly nextFromMs toMs
              return (entries ++ rest)

-- Parse Binance kline array: [[ts,"o","h","l","c","vol",closeTime,"quoteVol",...], ...]
-- Extracts close price (index 4) and quoteAssetVolume (index 7)
parseBinanceKlines :: Bool -> String -> [PriceEntry]
parseBinanceKlines isHourly s = case dropWhile (/= '[') s of
  '[':'[':rest -> parseKline rest
  _            -> []
  where
    mkLabel ts = let utc = msToUTC ts
                 in  if isHourly then showHour utc else showDay (utctDay utc)
    parseKline r =
      case reads r :: [(Double, String)] of
        [(ts, r1)] ->
          case skipQ (dropC r1) of                        -- skip open (1)
            Just r2 -> case skipQ (dropC r2) of           -- skip high (2)
              Just r3 -> case skipQ (dropC r3) of         -- skip low (3)
                Just r4 -> case readQ (dropC r4) of       -- read close (4)
                  Just (closeStr, r5) ->
                    case readQ (dropC r5) of              -- skip volume (5)
                      Just (_, r6) -> case skipNum (dropC r6) of  -- skip closeTime (6)
                        Just r7 -> case readQ (dropC r7) of      -- read quoteAssetVolume (7)
                          Just (volStr, _) ->
                            case (readMaybe' closeStr, readMaybe' volStr) of
                              (Just price, Just vol) ->
                                let label = mkLabel ts
                                in  (label, price, vol) : parseKline (skipNext r)
                              _ -> parseKline (skipNext r)
                          _ -> parseKline (skipNext r)
                        _ -> parseKline (skipNext r)
                      _ -> parseKline (skipNext r)
                  _ -> parseKline (skipNext r)
                _ -> parseKline (skipNext r)
              _ -> parseKline (skipNext r)
            _ -> parseKline (skipNext r)
        _ -> []
    dropC = drop 1 . dropWhile (/= ',')
    skipQ str = case dropWhile (/= '"') str of
      '"':rest -> Just (drop 1 (dropWhile (/= '"') rest))
      _        -> Nothing
    readQ str = case dropWhile (/= '"') str of
      '"':rest -> let (v, rest2) = span (/= '"') rest in Just (v, drop 1 rest2)
      _        -> Nothing
    skipNum str =
      let trimmed = dropWhile isSpace str
      in  case reads trimmed :: [(Double, String)] of
            [(_,rest)] -> Just rest
            _          -> Nothing
    skipNext str = case dropWhile (/= '[') str of
      '[':rest -> rest
      _        -> ""

extractQuotedValue :: String -> String -> Maybe String
extractQuotedValue _   [] = Nothing
extractQuotedValue key json
  | needle `isPrefixOf` json =
      let rest = drop (length needle) json
      in  Just (takeWhile (/= '"') rest)
  | otherwise = extractQuotedValue key (drop 1 json)
  where needle = "\"" ++ key ++ "\":\""

trimZeros :: String -> String
trimZeros s
  | '.' `elem` s = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse $ s
  | otherwise     = s

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

-------------------------------------------------------------------------------
-- Date helpers
-------------------------------------------------------------------------------

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

dayToEpoch :: Day -> Integer
dayToEpoch d = round $ utcTimeToPOSIXSeconds (UTCTime d 0)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%Y-%m-%d"

downsampleDaily :: [(Double, Double)] -> [(Double, Double)] -> [PriceEntry]
downsampleDaily priceEntries volumeEntries =
  let priceDays = map (\(ts, p) -> (showDay (utctDay (msToUTC ts)), p)) priceEntries
      volMap    = map (\(ts, v) -> (showDay (utctDay (msToUTC ts)), v)) volumeEntries
      grouped   = groupBy (\a b -> fst a == fst b) priceDays
      dailyP    = map (\grp -> (fst (head grp), snd (head grp))) grouped
      volGrouped = groupBy (\a b -> fst a == fst b) volMap
      dailyV    = map (\grp -> (fst (head grp), snd (head grp))) volGrouped
  in  map (\(d, p) -> (d, p, lookupVol d dailyV)) dailyP

downsampleHourly :: [(Double, Double)] -> [(Double, Double)] -> [PriceEntry]
downsampleHourly priceEntries volumeEntries =
  let priceHours = map (\(ts, p) -> (showHour (msToUTC ts), p)) priceEntries
      volMap     = map (\(ts, v) -> (showHour (msToUTC ts), v)) volumeEntries
      grouped    = groupBy (\a b -> fst a == fst b) priceHours
      hourlyP    = map (\grp -> (fst (head grp), snd (head grp))) grouped
      volGrouped = groupBy (\a b -> fst a == fst b) volMap
      hourlyV    = map (\grp -> (fst (head grp), snd (head grp))) volGrouped
  in  map (\(h, p) -> (h, p, lookupVol h hourlyV)) hourlyP

showHour :: UTCTime -> String
showHour = formatTime defaultTimeLocale "%Y-%m-%d %H:00"

labelToMs :: String -> Integer
labelToMs s
  | length s > 10 = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" s :: Maybe UTCTime of
      Just utc -> round (utcTimeToPOSIXSeconds utc) * 1000
      Nothing  -> 0
  | otherwise = case parseDay s of
      Just day -> dayToEpoch day * 1000
      Nothing  -> 0

lookupVol :: String -> [(String, Double)] -> Double
lookupVol _ [] = 0
lookupVol label ((l,v):rest)
  | label == l = v
  | otherwise  = lookupVol label rest

msToUTC :: Double -> UTCTime
msToUTC ms = posixSecondsToUTCTime (realToFrac (ms / 1000))

-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

showPrice :: Double -> String
showPrice p = printf "%.2f" p

showVolume :: Double -> String
showVolume v = printf "%.0f" v

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
  let (curUsd, curEur, curGbp, curVol) = case currentResult of
        Just x  -> x
        Nothing -> ("--", "--", "--", "--")

  -- Fetch historical prices
  case (parseDay fromStr, parseDay toStr) of
    (Just fromDay, Just toDay) | fromDay < toDay -> do
      let isHourly = diffDays toDay fromDay <= 30
      histResult <- fetchHistoricalPrices isHourly fromDay toDay
      case histResult of
        Left err -> do
          putStrLn $ "Error: " ++ err
        Right entries -> do
          let htmlPath = "/tmp/btcprice.html"
          writeFile htmlPath (generateHtml curUsd curEur curGbp curVol isHourly entries)
          putStrLn $ "Opening " ++ htmlPath ++ " in browser..."
          openBrowser htmlPath
    _ -> putStrLn "Error: invalid or reversed date range. Use: btcgui YYYY-MM-DD YYYY-MM-DD"

-------------------------------------------------------------------------------
-- HTML generation
-------------------------------------------------------------------------------

generateHtml :: String -> String -> String -> String -> Bool -> [PriceEntry] -> String
generateHtml curUsd curEur curGbp curVol isHourly entries =
  let prices    = map (\(_,p,_) -> p) entries
      volumes   = map (\(_,_,v) -> v) entries
      minP      = minimum prices
      maxP      = maximum prices
      avgP      = sum prices / fromIntegral (length prices)
      totalVol  = sum volumes
      avgVol    = totalVol / fromIntegral (length volumes)
      (_,firstP,_) = head entries
      (_,lastP,_)  = last entries
      changePct = (lastP - firstP) / firstP * 100 :: Double
      (firstLabel,_,_) = head entries
      (lastLabel,_,_)  = last entries

      labelsJs  = "[" ++ joinWith "," (map (\(l,_,_) -> "\"" ++ l ++ "\"") entries) ++ "]"
      pricesJs  = "[" ++ joinWith "," (map (\(_,p,_) -> printf "%.2f" p) entries) ++ "]"
      volumesJs = "[" ++ joinWith "," (map (\(_,_,v) -> printf "%.0f" v) entries) ++ "]"

      tableRows = concatMap makeRow (addChanges entries)
      unitLabel = if isHourly then "hours" else "days"
      volAvgLabel = if isHourly then "Avg Hourly Vol" else "Avg Daily Vol"
      tableTitle = if isHourly then "Hourly Prices" else "Daily Prices"
      defaultGran = if isHourly then "hourly" else "daily"
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
  ++ volChip curVol
  ++ "</div></div>"

  -- Date range controls
  ++ "<div class='section'>"
  ++ "<div class='range-bar'>"
  ++ "<label class='range-label'>From</label>"
  ++ "<input type='date' id='fromDate' value='" ++ take 10 firstLabel ++ "' class='date-input'>"
  ++ "<label class='range-label'>To</label>"
  ++ "<input type='date' id='toDate' value='" ++ take 10 lastLabel ++ "' class='date-input'>"
  ++ "<button id='fetchBtn' class='btn' onclick='fetchRange()'>Fetch</button>"
  ++ "<span id='status' class='status'></span>"
  ++ "</div>"
  ++ "<div class='preset-bar'>"
  ++ "<button class='preset range-btn' onclick='setPreset(1,this)'>1D</button>"
  ++ "<button class='preset range-btn' onclick='setPreset(7,this)'>1W</button>"
  ++ "<button class='preset range-btn' onclick='setPreset(30,this)'>1M</button>"
  ++ "<button class='preset range-btn active' id='btn3m' onclick='setPreset(90,this)'>3M</button>"
  ++ "<button class='preset range-btn' onclick='setPreset(365,this)'>1Y</button>"
  ++ "<button class='preset range-btn' onclick='setPreset(0,this)'>All</button>"
  ++ "<span class='preset-sep'></span>"
  ++ "<button class='preset' id='plBtn' onclick='togglePowerLaw()'>Power Law</button>"
  ++ "<button class='preset' id='logBtn' onclick='toggleLog()'>Log Scale</button>"
  ++ "<button class='preset' id='llBtn' onclick='toggleLogLog()'>Log-Log</button>"
  ++ "</div></div>"

  -- Stats cards
  ++ "<div class='section' id='statsSection'>"
  ++ "<h2 id='periodHeader'>Period: " ++ firstLabel ++ " &rarr; " ++ lastLabel
  ++ "  <span class='day-count'>(" ++ show (length entries) ++ " " ++ unitLabel ++ ")</span></h2>"
  ++ "<div class='stats-grid' id='statsGrid'>"
  ++ statCard "High"    ("$" ++ formatNum (showPrice maxP)) ""
  ++ statCard "Low"     ("$" ++ formatNum (showPrice minP)) ""
  ++ statCard "Average" ("$" ++ formatNum (showPrice avgP)) ""
  ++ statCard "Change"  (printf "%+.2f%%" changePct)
              (if changePct >= 0 then "positive" else "negative")
  ++ statCard volAvgLabel ("$" ++ formatNum (showVolume avgVol)) ""
  ++ statCard "Total Vol" ("$" ++ formatNum (showVolume totalVol)) ""
  ++ "</div></div>"

  -- Chart
  ++ "<div class='section'>"
  ++ "<canvas id='priceChart'></canvas>"
  ++ "</div>"

  -- Table
  ++ "<div class='section'>"
  ++ "<h2 id='tableHeader'>" ++ tableTitle ++ "</h2>"
  ++ "<div class='table-scroll'>"
  ++ "<table class='price-table'>"
  ++ "<thead><tr><th class='sortable' onclick='toggleDateSort()'>Date <span id='sortArrow' style='font-size:9px;opacity:0.5'>&#9650;</span></th><th>Price (USD)</th><th>Volume (USD)</th><th>Change</th></tr></thead>"
  ++ "<tbody id='tableBody'>" ++ tableRows ++ "</tbody></table></div></div>"

  ++ "</div>"

  -- Chart.js init + interactive fetch logic
  ++ "<script>"
  ++ "var priceChart=null;var tableSortAsc=true;var showPL=false;var showLog=false;var showLL=false;"
  ++ "var curLabels=[],curData=[],curVols=[];"
  ++ "var curGranularity='" ++ defaultGran ++ "';"
  ++ "var GENESIS=new Date('2009-01-03T00:00:00Z').getTime();"

  ++ "function getInterval(f,t){"
  ++ "var diffDays=(new Date(t+'T00:00:00Z')-new Date(f+'T00:00:00Z'))/86400000;"
  ++ "return diffDays<=30?'hourly':'daily';}"

  ++ "function dateToDays(ds){"
  ++ "var t=ds.length>10?new Date(ds.replace(' ','T')+':00Z').getTime():new Date(ds+'T00:00:00Z').getTime();"
  ++ "return (t-GENESIS)/86400000;}"

  ++ "function calcPowerLaw(labels){"
  ++ "var support=[],nearSup=[],fair=[],resist=[];"
  ++ "labels.forEach(function(ds){"
  ++ "var days=dateToDays(ds);"
  ++ "if(days<=0){support.push(null);nearSup.push(null);fair.push(null);resist.push(null);return;}"
  ++ "var ld=Math.log10(days);"
  ++ "support.push(Math.pow(10,-17.51+5.82*ld));"
  ++ "nearSup.push(Math.pow(10,-17.35+5.82*ld));"
  ++ "fair.push(Math.pow(10,-17.01+5.82*ld));"
  ++ "resist.push(Math.pow(10,-16.51+5.82*ld));});"
  ++ "return {support:support,nearSup:nearSup,fair:fair,resist:resist};}"

  ++ "function calcPowerLawXY(labels){"
  ++ "var support=[],nearSup=[],fair=[],resist=[];"
  ++ "labels.forEach(function(ds){"
  ++ "var days=dateToDays(ds);"
  ++ "if(days<=0)return;"
  ++ "var ld=Math.log10(days);"
  ++ "support.push({x:days,y:Math.pow(10,-17.51+5.82*ld)});"
  ++ "nearSup.push({x:days,y:Math.pow(10,-17.35+5.82*ld)});"
  ++ "fair.push({x:days,y:Math.pow(10,-17.01+5.82*ld)});"
  ++ "resist.push({x:days,y:Math.pow(10,-16.51+5.82*ld)});});"
  ++ "return {support:support,nearSup:nearSup,fair:fair,resist:resist};}"

  ++ "function getChartOpts(){"
  ++ "if(showLL)return getLogLogOpts();"
  ++ "return {responsive:true,interaction:{mode:'index',intersect:false},"
  ++ "plugins:{legend:{labels:{color:'#ccc',font:{size:14}}},"
  ++ "tooltip:{backgroundColor:'#1e1e3a',titleColor:'#f7931a',bodyColor:'#e0e0e0',"
  ++ "borderColor:'#f7931a',borderWidth:1,padding:12,"
  ++ "callbacks:{label:function(c){"
  ++ "if(c.dataset.yAxisID==='y1')return ' Vol: $'+Number(c.raw).toLocaleString(undefined,{maximumFractionDigits:0});"
  ++ "if(c.raw===null)return null;"
  ++ "return ' '+c.dataset.label+': $'+Number(c.raw).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2});},"
  ++ "labelTextColor:function(c){return c.dataset.borderColor||'#e0e0e0';}"
  ++ "}}},"
  ++ "scales:{x:{ticks:{color:'#888',maxTicksLimit:12,font:{size:11}},"
  ++ "grid:{color:'rgba(255,255,255,0.05)'}},"
  ++ "y:{type:showLog?'logarithmic':'linear',position:'left',ticks:{color:'#888',font:{size:11},"
  ++ "callback:function(v){return '$'+v.toLocaleString()}},"
  ++ "grid:{color:'rgba(255,255,255,0.05)'}},"
  ++ "y1:{position:'right',ticks:{color:'#555',font:{size:10},"
  ++ "callback:function(v){if(v>=1e9)return '$'+Math.round(v/1e9)+'B';"
  ++ "if(v>=1e6)return '$'+Math.round(v/1e6)+'M';return '$'+v.toLocaleString()}},"
  ++ "grid:{drawOnChartArea:false}}}}};"

  ++ "function getLogLogOpts(){"
  ++ "return {responsive:true,interaction:{mode:'x',intersect:false},"
  ++ "plugins:{legend:{labels:{color:'#ccc',font:{size:14}}},"
  ++ "tooltip:{backgroundColor:'#1e1e3a',titleColor:'#f7931a',bodyColor:'#e0e0e0',"
  ++ "borderColor:'#f7931a',borderWidth:1,padding:12,"
  ++ "filter:function(item,idx,items){return items.findIndex(function(i){return i.datasetIndex===item.datasetIndex;})===idx;},"
  ++ "callbacks:{title:function(items){"
  ++ "if(!items.length)return '';"
  ++ "var days=items[0].raw.x;"
  ++ "var date=new Date(GENESIS+days*86400000).toISOString().slice(0,10);"
  ++ "return date+'  (day '+Math.round(days)+')';},"
  ++ "label:function(c){"
  ++ "if(c.raw===null||c.raw.y===undefined)return null;"
  ++ "return ' '+c.dataset.label+': $'+Number(c.raw.y).toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2});},"
  ++ "labelTextColor:function(c){return c.dataset.borderColor||'#e0e0e0';}"
  ++ "}}},"
  ++ "scales:{x:{type:'logarithmic',position:'bottom',"
  ++ "title:{display:true,text:'Days since genesis (Jan 3, 2009)',color:'#888',font:{size:12}},"
  ++ "ticks:{color:'#888',font:{size:11},callback:function(v){return v.toLocaleString()}},"
  ++ "grid:{color:'rgba(255,255,255,0.05)'}},"
  ++ "y:{type:'logarithmic',position:'left',"
  ++ "title:{display:true,text:'Price (USD)',color:'#888',font:{size:12}},"
  ++ "ticks:{color:'#888',font:{size:11},"
  ++ "callback:function(v){if(v>=1000)return '$'+Math.round(v).toLocaleString();return '$'+v;}},"
  ++ "grid:{color:'rgba(255,255,255,0.05)'}}}}};"

  ++ "function extendLabels(labels,frac){"
  ++ "var n=labels.length;if(n<2)return labels;"
  ++ "var extra=Math.ceil(n*frac);"
  ++ "var isHourly=labels[0].length>10;"
  ++ "var step=isHourly?3600000:86400000;"
  ++ "var lastStr=isHourly?labels[n-1].replace(' ','T')+':00Z':labels[n-1]+'T00:00:00Z';"
  ++ "var last=new Date(lastStr);"
  ++ "var ext=labels.slice();"
  ++ "for(var i=1;i<=extra;i++){var d=new Date(last.getTime()+i*step);"
  ++ "ext.push(isHourly?d.toISOString().slice(0,13).replace('T',' ')+':00':d.toISOString().slice(0,10));}return ext;}"

  ++ "function renderChart(labels,data,vols){"
  ++ "curLabels=labels;curData=data;curVols=vols;"
  ++ "if(priceChart)priceChart.destroy();"
  ++ "var ctx=document.getElementById('priceChart').getContext('2d');"
  ++ "if(showLL){renderLogLog(ctx,labels,data);return;}"
  ++ "var chartLabels=labels,chartData=data,chartVols=vols;"
  ++ "if(showPL){chartLabels=extendLabels(labels,0.2);"
  ++ "var pad=chartLabels.length-labels.length;"
  ++ "chartData=data.concat(Array(pad).fill(null));"
  ++ "chartVols=vols?vols.concat(Array(pad).fill(null)):vols;}"
  ++ "var datasets=[{label:'BTC Price',data:chartData,type:'line',yAxisID:'y',"
  ++ "borderColor:'#f7931a',backgroundColor:'rgba(247,147,26,0.08)',"
  ++ "borderWidth:2,fill:true,tension:0.2,pointRadius:0,pointHitRadius:10,"
  ++ "pointHoverRadius:5,pointHoverBackgroundColor:'#f7931a',order:1}];"
  ++ "if(chartVols&&chartVols.length>0){datasets.push({label:'Volume',data:chartVols,type:'bar',yAxisID:'y1',"
  ++ "backgroundColor:'rgba(78,186,234,0.35)',borderColor:'rgba(78,186,234,0.5)',"
  ++ "borderWidth:1,order:0});}"
  ++ "if(showPL){var pl=calcPowerLaw(chartLabels);"
  ++ "datasets.push({label:'PL Support',data:pl.support,type:'line',yAxisID:'y',"
  ++ "borderColor:'#2ecc71',borderWidth:1.5,borderDash:[6,4],pointRadius:0,fill:false,tension:0.4,order:0});"
  ++ "datasets.push({label:'PL Near Support',data:pl.nearSup,type:'line',yAxisID:'y',"
  ++ "borderColor:'#82e0aa',borderWidth:1.5,borderDash:[3,3],pointRadius:0,fill:false,tension:0.4,order:0});"
  ++ "datasets.push({label:'PL Fair Value',data:pl.fair,type:'line',yAxisID:'y',"
  ++ "borderColor:'rgba(255,255,255,0.6)',borderWidth:2,pointRadius:0,fill:false,tension:0.4,order:0});"
  ++ "datasets.push({label:'PL Resistance',data:pl.resist,type:'line',yAxisID:'y',"
  ++ "borderColor:'#e74c3c',borderWidth:1.5,borderDash:[6,4],pointRadius:0,fill:false,tension:0.4,order:0});}"
  ++ "priceChart=new Chart(ctx,{type:'line',data:{labels:chartLabels,datasets:datasets},"
  ++ "options:getChartOpts()});}"

  ++ "function renderLogLog(ctx,labels,data){"
  ++ "var priceXY=[];labels.forEach(function(ds,i){"
  ++ "var days=dateToDays(ds);if(days>0)priceXY.push({x:days,y:data[i]});});"
  ++ "var datasets=[{label:'BTC Price',data:priceXY,type:'scatter',"
  ++ "borderColor:'#f7931a',backgroundColor:'rgba(247,147,26,0.6)',"
  ++ "pointRadius:2,pointHitRadius:6,order:1}];"
  ++ "if(showPL){var extLabels=extendLabels(labels,0.2);"
  ++ "var pl=calcPowerLawXY(extLabels);"
  ++ "datasets.push({label:'PL Support',data:pl.support,type:'line',"
  ++ "borderColor:'#2ecc71',borderWidth:1.5,borderDash:[6,4],pointRadius:0,fill:false,tension:0,order:0});"
  ++ "datasets.push({label:'PL Near Support',data:pl.nearSup,type:'line',"
  ++ "borderColor:'#82e0aa',borderWidth:1.5,borderDash:[3,3],pointRadius:0,fill:false,tension:0,order:0});"
  ++ "datasets.push({label:'PL Fair Value',data:pl.fair,type:'line',"
  ++ "borderColor:'rgba(255,255,255,0.6)',borderWidth:2,pointRadius:0,fill:false,tension:0,order:0});"
  ++ "datasets.push({label:'PL Resistance',data:pl.resist,type:'line',"
  ++ "borderColor:'#e74c3c',borderWidth:1.5,borderDash:[6,4],pointRadius:0,fill:false,tension:0,order:0});}"
  ++ "priceChart=new Chart(ctx,{type:'scatter',data:{datasets:datasets},"
  ++ "options:getLogLogOpts()});}"

  ++ "function togglePowerLaw(){showPL=!showPL;"
  ++ "document.getElementById('plBtn').classList.toggle('active');"
  ++ "renderChart(curLabels,curData,curVols);}"
  ++ "function toggleLog(){showLog=!showLog;"
  ++ "document.getElementById('logBtn').classList.toggle('active');"
  ++ "renderChart(curLabels,curData,curVols);}"
  ++ "function toggleLogLog(){showLL=!showLL;"
  ++ "document.getElementById('llBtn').classList.toggle('active');"
  ++ "renderChart(curLabels,curData,curVols);}"

  ++ "function fmt(n){return n.toLocaleString(undefined,{minimumFractionDigits:2,maximumFractionDigits:2})}"
  ++ "function fmtVol(n){return Math.round(n).toLocaleString()}"

  ++ "function updateStats(daily){"
  ++ "var prices=daily.map(function(d){return d[1]});"
  ++ "var vols=daily.map(function(d){return d[2]||0});"
  ++ "var hi=Math.max.apply(null,prices),lo=Math.min.apply(null,prices);"
  ++ "var avg=prices.reduce(function(a,b){return a+b},0)/prices.length;"
  ++ "var totalVol=vols.reduce(function(a,b){return a+b},0);"
  ++ "var avgVol=totalVol/vols.length;"
  ++ "var chg=(prices[prices.length-1]-prices[0])/prices[0]*100;"
  ++ "var cls=chg>=0?'positive':'negative';"
  ++ "var unit=curGranularity==='hourly'?'hours':'days';"
  ++ "var volLabel=curGranularity==='hourly'?'Avg Hourly Vol':'Avg Daily Vol';"
  ++ "document.getElementById('periodHeader').innerHTML="
  ++ "'Period: '+daily[0][0]+' &rarr; '+daily[daily.length-1][0]"
  ++ "+'  <span class=\\'day-count\\'>('+daily.length+' '+unit+')</span>';"
  ++ "document.getElementById('statsGrid').innerHTML="
  ++ "sc('High','$'+fmt(hi),'')+sc('Low','$'+fmt(lo),'')"
  ++ "+sc('Average','$'+fmt(avg),'')+sc('Change',(chg>=0?'+':'')+chg.toFixed(2)+'%',cls)"
  ++ "+sc(volLabel,'$'+fmtVol(avgVol),'')+sc('Total Vol','$'+fmtVol(totalVol),'');}"

  ++ "function sc(l,v,c){"
  ++ "return '<div class=\\'stat-card\\'><div class=\\'stat-label\\'>'+l+'</div>"
  ++ "<div class=\\'stat-value '+c+'\\'>'+v+'</div></div>';}"

  ++ "var curDaily=[];"
  ++ "function updateTable(daily){curDaily=daily;renderTable();}"
  ++ "function renderTable(){"
  ++ "var rows=curDaily.slice();if(!tableSortAsc)rows.reverse();"
  ++ "var chronological=tableSortAsc?rows:rows.slice().reverse();"
  ++ "var changeMap={};var cprev=null;"
  ++ "chronological.forEach(function(d){if(cprev!==null){var pct=(d[1]-cprev)/cprev*100;changeMap[d[0]]=pct;}cprev=d[1];});"
  ++ "var html='';"
  ++ "rows.forEach(function(d){"
  ++ "var date=d[0],price=d[1],vol=d[2]||0,ch='',cls='';"
  ++ "if(changeMap[date]!==undefined){var pct=changeMap[date];cls=pct>=0?'positive':'negative';"
  ++ "ch=(pct>=0?'+':'')+pct.toFixed(2)+'%';}"
  ++ "html+='<tr><td>'+date+'</td><td class=\\'price-cell\\'>$'+fmt(price)+'</td>"
  ++ "<td class=\\'vol-cell\\'>$'+fmtVol(vol)+'</td>"
  ++ "<td class=\\''+cls+'\\'>'+ch+'</td></tr>';});"
  ++ "document.getElementById('tableBody').innerHTML=html;"
  ++ "document.getElementById('sortArrow').innerHTML=tableSortAsc?'&#9650;':'&#9660;';}"
  ++ "function toggleDateSort(){tableSortAsc=!tableSortAsc;renderTable();}"

  ++ "function downsample(prices,volumes){"
  ++ "var hourly=curGranularity==='hourly';"
  ++ "var byKey={};prices.forEach(function(p){"
  ++ "var dt=new Date(p[0]);"
  ++ "var key=hourly?dt.toISOString().slice(0,13).replace('T',' ')+':00':dt.toISOString().slice(0,10);"
  ++ "if(!byKey[key])byKey[key]={price:p[1],vol:0};});"
  ++ "if(volumes)volumes.forEach(function(v){"
  ++ "var dt=new Date(v[0]);"
  ++ "var key=hourly?dt.toISOString().slice(0,13).replace('T',' ')+':00':dt.toISOString().slice(0,10);"
  ++ "if(byKey[key])byKey[key].vol=v[1];});"
  ++ "return Object.keys(byKey).sort().map(function(k){return [k,byKey[k].price,byKey[k].vol]});}"

  ++ "function showDaily(daily){"
  ++ "document.getElementById('tableHeader').textContent=curGranularity==='hourly'?'Hourly Prices':'Daily Prices';"
  ++ "renderChart(daily.map(function(d){return d[0]}),daily.map(function(d){return d[1]}),daily.map(function(d){return d[2]||0}));"
  ++ "updateStats(daily);updateTable(daily);}"

  ++ "function fetchBinanceChunks(fromMs,toMs,acc){"
  ++ "if(fromMs>=toMs){if(acc.length===0){document.getElementById('status').textContent='No data from Binance either';"
  ++ "document.getElementById('fetchBtn').disabled=false;return;}"
  ++ "showDaily(acc);document.getElementById('status').textContent='(via Binance)';"
  ++ "document.getElementById('fetchBtn').disabled=false;return;}"
  ++ "var hourly=curGranularity==='hourly';"
  ++ "var interval=hourly?'1h':'1d';"
  ++ "fetch('https://api.binance.com/api/v3/klines?symbol=BTCUSDT&interval='+interval+'&startTime='+fromMs+'&endTime='+toMs+'&limit=1000')"
  ++ ".then(function(r){return r.json()})"
  ++ ".then(function(klines){"
  ++ "if(!Array.isArray(klines)||klines.length===0){"
  ++ "if(acc.length===0){document.getElementById('status').textContent='No data from Binance either';"
  ++ "document.getElementById('fetchBtn').disabled=false;return;}"
  ++ "showDaily(acc);document.getElementById('status').textContent='(via Binance)';"
  ++ "document.getElementById('fetchBtn').disabled=false;return;}"
  ++ "var chunk=klines.map(function(k){"
  ++ "var dt=new Date(k[0]);"
  ++ "var label=hourly?dt.toISOString().slice(0,13).replace('T',' ')+':00':dt.toISOString().slice(0,10);"
  ++ "return [label,parseFloat(k[4]),parseFloat(k[7])];});"
  ++ "var all=acc.concat(chunk);"
  ++ "var unit=hourly?'hours':'days';"
  ++ "var step=hourly?3600000:86400000;"
  ++ "if(klines.length<1000){showDaily(all);document.getElementById('status').textContent='(via Binance)';"
  ++ "document.getElementById('fetchBtn').disabled=false;}"
  ++ "else{var lastTs=klines[klines.length-1][0]+step;"
  ++ "document.getElementById('status').textContent='Fetching from Binance ('+all.length+' '+unit+')...';"
  ++ "fetchBinanceChunks(lastTs,toMs,all);}})"
  ++ ".catch(function(e){document.getElementById('status').textContent='Both APIs failed: '+e;"
  ++ "document.getElementById('fetchBtn').disabled=false;});}"

  ++ "function fetchBinance(fromMs,toMs){"
  ++ "document.getElementById('status').textContent='CoinGecko failed, trying Binance...';"
  ++ "fetchBinanceChunks(fromMs,toMs,[]);}"

  ++ "function fetchRange(){"
  ++ "var f=document.getElementById('fromDate').value;"
  ++ "var t=document.getElementById('toDate').value;"
  ++ "if(!f||!t){document.getElementById('status').textContent='Pick both dates';return;}"
  ++ "var from=Math.floor(new Date(f+'T00:00:00Z').getTime()/1000);"
  ++ "var to=Math.floor(new Date(t+'T00:00:00Z').getTime()/1000)+86400;"
  ++ "if(from>=to){document.getElementById('status').textContent='From must be before To';return;}"
  ++ "curGranularity=getInterval(f,t);"
  ++ "document.getElementById('status').textContent='Fetching...';"
  ++ "document.getElementById('fetchBtn').disabled=true;"
  ++ "var fromMs=from*1000,toMs=to*1000;"
  ++ "fetch('https://api.coingecko.com/api/v3/coins/bitcoin/market_chart/range"
  ++ "?vs_currency=usd&from='+from+'&to='+to)"
  ++ ".then(function(r){if(!r.ok)throw new Error(r.status);return r.json()})"
  ++ ".then(function(j){"
  ++ "if(j.error){fetchBinance(fromMs,toMs);return;}"
  ++ "var daily=downsample(j.prices,j.total_volumes);"
  ++ "if(daily.length===0){fetchBinance(fromMs,toMs);return;}"
  ++ "showDaily(daily);document.getElementById('status').textContent='';"
  ++ "document.getElementById('fetchBtn').disabled=false;})"
  ++ ".catch(function(e){fetchBinance(fromMs,toMs);});}"

  -- Render initial chart with Haskell-provided data
  ++ "function clearRangeActive(){document.querySelectorAll('.range-btn').forEach(function(b){b.classList.remove('active');});}"
  ++ "function setPreset(days,btn){"
  ++ "clearRangeActive();btn.classList.add('active');"
  ++ "var to=new Date();var from;"
  ++ "if(days===0){from=new Date('2010-07-17');}else{"
  ++ "from=new Date(to);from.setDate(from.getDate()-days);}"
  ++ "document.getElementById('fromDate').value=from.toISOString().slice(0,10);"
  ++ "document.getElementById('toDate').value=to.toISOString().slice(0,10);"
  ++ "fetchRange();}"

  ++ "renderChart(" ++ labelsJs ++ "," ++ pricesJs ++ "," ++ volumesJs ++ ");"
  ++ "</script>"
  ++ "</body></html>"

priceChip :: String -> String -> String -> String
priceChip label symbol val =
     "<div class='price-chip'>"
  ++ "<span class='chip-label'>" ++ label ++ "</span>"
  ++ "<span class='chip-value'>" ++ symbol ++ formatNum val ++ "</span>"
  ++ "</div>"

volChip :: String -> String
volChip val =
     "<div class='price-chip vol-chip'>"
  ++ "<span class='chip-label'>24h Volume</span>"
  ++ "<span class='chip-value'>$" ++ formatNum (formatVolStr val) ++ "</span>"
  ++ "</div>"

formatVolStr :: String -> String
formatVolStr s = let (whole, _) = break (== '.') s in whole

statCard :: String -> String -> String -> String
statCard label value cls =
     "<div class='stat-card'>"
  ++ "<div class='stat-label'>" ++ label ++ "</div>"
  ++ "<div class='stat-value " ++ cls ++ "'>" ++ value ++ "</div>"
  ++ "</div>"

addChanges :: [PriceEntry] -> [(String, String, String, String, String)]
addChanges [] = []
addChanges ((label,p,v):es) = (label, "$" ++ formatNum (showPrice p), "$" ++ formatNum (showVolume v), "", "") : go p es
  where
    go _    []              = []
    go prev ((l,p',v'):xs) =
      let pct = (p' - prev) / prev * 100 :: Double
          cls = if pct >= 0 then "positive" else "negative"
      in  (l, "$" ++ formatNum (showPrice p'), "$" ++ formatNum (showVolume v'), printf "%+.2f%%" pct, cls) : go p' xs

makeRow :: (String, String, String, String, String) -> String
makeRow (date, price, vol, change, cls) =
     "<tr><td>" ++ date ++ "</td>"
  ++ "<td class='price-cell'>" ++ price ++ "</td>"
  ++ "<td class='vol-cell'>" ++ vol ++ "</td>"
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
  , ".range-bar{display:flex;align-items:center;gap:12px;flex-wrap:wrap}"
  , ".range-label{color:#888;font-size:13px}"
  , ".date-input{background:#0f3460;border:1px solid #333;color:#e0e0e0;padding:8px 12px;border-radius:8px;font-size:14px;font-family:inherit}"
  , ".date-input::-webkit-calendar-picker-indicator{filter:invert(0.7)}"
  , ".btn{background:#f7931a;color:#fff;border:none;padding:10px 22px;border-radius:8px;cursor:pointer;font-size:14px;font-weight:600;font-family:inherit;transition:background 0.15s}"
  , ".btn:hover{background:#e8851a}"
  , ".btn:disabled{opacity:0.5;cursor:not-allowed}"
  , ".status{color:#f7931a;font-size:13px}"
  , ".preset-bar{display:flex;gap:8px;margin-top:12px}"
  , ".preset{background:#0f3460;color:#ccc;border:1px solid #333;padding:6px 16px;border-radius:6px;cursor:pointer;font-size:13px;font-weight:600;font-family:inherit;transition:all 0.15s}"
  , ".preset:hover{background:#f7931a;color:#fff;border-color:#f7931a}"
  , ".preset.active{background:#f7931a;color:#fff;border-color:#f7931a}"
  , ".preset-sep{width:1px;height:20px;background:#333;margin:0 4px}"
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
  , ".sortable{cursor:pointer;user-select:none}.sortable:hover{color:#f7931a}"
  , ".price-table td{padding:7px 14px;border-bottom:1px solid #1a1a3e}"
  , ".price-table tr:hover{background:#0f3460}"
  , ".price-cell{font-variant-numeric:tabular-nums;font-weight:500}"
  , ".vol-cell{font-variant-numeric:tabular-nums;color:#4ebaea;font-size:12px}"
  , ".vol-chip .chip-value{color:#4ebaea}"
  , "::-webkit-scrollbar{width:8px}"
  , "::-webkit-scrollbar-track{background:#0f0f23;border-radius:4px}"
  , "::-webkit-scrollbar-thumb{background:#333;border-radius:4px}"
  ]
