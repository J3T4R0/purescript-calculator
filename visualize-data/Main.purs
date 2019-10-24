{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import System.Directory
import Network.Wreq
import Control.Lens
import Data.Fixed
import Data.Time
import Data.Dates
import Data.Hashable
import Data.Hashable.Time
import qualified Data.List as DL
import qualified Data.HashMap as DHM
import qualified Data.ByteString.Lazy as DBSL
import qualified Data.Sequence as DSeq
import Data.Aeson
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

-------------------------------------------------------------------------------

data ComplaintEntry =
  ComplaintEntry {
      unique_key :: String
    , created_date :: String
    , complaint_type :: String
    , borough :: String
    , location_type :: Maybe String
    , incident_address :: Maybe String
    , incident_zip :: String
  } deriving (Show, Generic)

instance FromJSON ComplaintEntry where
  parseJSON (Object v) =
    ComplaintEntry  <$> v .:  "unique_key"
                    <*> v .:  "created_date"
                    <*> v .:  "complaint_type"
                    <*> v .:  "borough"
                    <*> v .:? "location_type"
                    <*> v .:? "incident_address"
                    <*> v .:  "incident_zip"

instance ToJSON ComplaintEntry

-------------------------------------------------------------------------------

main :: IO ()
main = do
  complaints <- getComplaints
  print $ length complaints
  print $ minCreatedDate complaints
  print $ maxCreatedDate complaints
  renderableToFile def "./charts/complaintCountsPerDayOfWeek.png"  (chartDaysOfWeekComplaintCounts complaints)
  renderableToFile def "./charts/boroughsPopSizes.png"             (chartBoroughsPopSizes complaints)
  renderableToFile def "./charts/yearMonthCounts.png"              (chartComplaintsCountByYearMonth complaints)
  renderableToFile def "./charts/yearMonthCounts2015.png"          (chartComplaintsCountByYearMonth2015 complaints)
  renderableToFile def "./charts/boroughsCounts.png"               (chartComplaintsCountByBoroughs complaints)
  renderableToFile def "./charts/boroughsCounts2015.png"           (chartComplaintsCountByBoroughs2015 complaints)
  renderableToFile def "./charts/createdDatesCounts_month.png"     (chartComplaintsCountByCreatedDatesAtMonth complaints)
  renderableToFile def "./charts/createdDatesCounts_day.png"       (chartComplaintsCountByCreatedDatesAtDay complaints)
  renderableToFile def "./charts/createdDatesCounts_month_day.png" (chartComplaintsCountByCreatedDatesAtMonthDay complaints)
  return ()

-------------------------------------------------------------------------------

complaintEntryMaybeValue :: (ComplaintEntry -> Maybe String) -> ComplaintEntry -> String
complaintEntryMaybeValue f complaint = case f complaint of
                                        Nothing -> ""
                                        Just s  -> s

locationType :: ComplaintEntry -> String
locationType = complaintEntryMaybeValue location_type

incidentAddress :: ComplaintEntry -> String
incidentAddress = complaintEntryMaybeValue incident_address

complaintEntryValuesWFilter :: ([ComplaintEntry] -> [ComplaintEntry]) -> (ComplaintEntry -> String) -> [ComplaintEntry] -> [String]
complaintEntryValuesWFilter _ _ []    = []
complaintEntryValuesWFilter f g (x:y) = map g $ f (x:y)

complaintEntryValues :: (ComplaintEntry -> String) -> [ComplaintEntry] -> [String]
complaintEntryValues = complaintEntryValuesWFilter (filter (\x -> True))

filterCreatedDateYear :: String -> [ComplaintEntry] -> [ComplaintEntry]
filterCreatedDateYear year = filter ((DL.isInfixOf year) . created_date)

filterCreatedDate2015 :: [ComplaintEntry] -> [ComplaintEntry]
filterCreatedDate2015 = filterCreatedDateYear "2015"

boroughs :: [ComplaintEntry] -> [String]
boroughs = complaintEntryValues borough

boroughs2015 :: [ComplaintEntry] -> [String]
boroughs2015 = complaintEntryValuesWFilter filterCreatedDate2015 borough

createdDates :: [ComplaintEntry] -> [String]
createdDates = complaintEntryValues created_date

createdDates2015 :: [ComplaintEntry] -> [String]
createdDates2015 = complaintEntryValuesWFilter filterCreatedDate2015 created_date

yearMonths :: [String] -> [String]
yearMonths = map (take 7)

daysOfWeek :: [String]
daysOfWeek = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]

createdDatesYearMonths :: ([ComplaintEntry] -> [String]) -> [ComplaintEntry] -> [String]
createdDatesYearMonths _ [] = []
createdDatesYearMonths f (x:y)= yearMonths $ f (x:y)

createdDatesYearMonthsAll :: [ComplaintEntry] -> [String]
createdDatesYearMonthsAll = createdDatesYearMonths createdDates

createdDatesYearMonths2015 :: [ComplaintEntry] -> [String]
createdDatesYearMonths2015 = createdDatesYearMonths createdDates2015

-------------------------------------------------------------------------------

minMaxCreatedDate :: ([String] -> String) -> [ComplaintEntry] -> String
minMaxCreatedDate _ [] = ""
minMaxCreatedDate f (x:y) = f $ createdDates (x:y)

minCreatedDate :: [ComplaintEntry] -> String
minCreatedDate []    = ""
minCreatedDate (x:y) = minMaxCreatedDate minimum (x:y)

maxCreatedDate :: [ComplaintEntry] -> String
maxCreatedDate []    = ""
maxCreatedDate (x:y) = minMaxCreatedDate maximum (x:y)

complaintsCountByHashable :: (Hashable k, Ord k) => ([ComplaintEntry] -> [k]) -> [ComplaintEntry] -> [(k, Int)]
complaintsCountByHashable f = DL.sortOn fst . DHM.assocs . hashableCounts . f

complaintsCountByBoroughs :: [ComplaintEntry] -> [(String, Int)]
complaintsCountByBoroughs = complaintsCountByHashable boroughs

complaintsCountByBoroughs2015 :: [ComplaintEntry] -> [(String, Int)]
complaintsCountByBoroughs2015 = complaintsCountByHashable boroughs2015

complaintsCountByYearMonth :: [ComplaintEntry] -> [(String, Int)]
complaintsCountByYearMonth = complaintsCountByHashable createdDatesYearMonthsAll

complaintsCountByYearMonth2015 :: [ComplaintEntry] -> [(String, Int)]
complaintsCountByYearMonth2015 = complaintsCountByHashable createdDatesYearMonths2015

complaintsCountByCreatedDatesAtTrunc :: DTFmtTrunc -> [ComplaintEntry] -> [(LocalTime, Int)]
complaintsCountByCreatedDatesAtTrunc trunc = complaintsCountByHashable createdDates'
  where
    createdDates' :: [ComplaintEntry] -> [LocalTime]
    createdDates' = map (dateTimeStringToLocalTime trunc) . createdDates

complaintsCountByCreatedDatesAtMonth :: [ComplaintEntry] -> [(LocalTime, Int)]
complaintsCountByCreatedDatesAtMonth = complaintsCountByCreatedDatesAtTrunc AtMonth

complaintsCountByCreatedDatesAtDay :: [ComplaintEntry] -> [(LocalTime, Int)]
complaintsCountByCreatedDatesAtDay = complaintsCountByCreatedDatesAtTrunc AtDay

complaintsCountsPerDayOfWeek :: [ComplaintEntry] -> [(String, [Int])]
complaintsCountsPerDayOfWeek (x:y) = daysOfWeekCounts
  where
    complaints = (x:y)
    dayCounts = complaintsCountByCreatedDatesAtDay complaints

    localTimeToDayOfWeek :: LocalTime -> String
    localTimeToDayOfWeek lt = show $ dateWeekDay $ dayToDateTime $ localDay lt

    filterDayOfWeek :: String -> [(LocalTime, Int)] -> [(LocalTime, Int)]
    filterDayOfWeek s = filter (\ x -> localTimeToDayOfWeek (fst x) == s)

    dayOfWeekCount :: String -> [Int]
    dayOfWeekCount s = map snd $ filterDayOfWeek s dayCounts

    daysOfWeekCounts = map (\ x -> (x, dayOfWeekCount x)) daysOfWeek

-------------------------------------------------------------------------------

boroughsPopSizes :: [(String, Int)]
boroughsPopSizes = zip boroughNames popSizes2015
  where
    boroughNames = ["BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"]
    --              Bronx    Brookly  Manhatt  Queens   SI
    popSizes2014 = [1438159, 2621793, 1636268, 2321580, 473279]
    popSizes2015 = [1455444, 2636735, 1644518, 2399150, 474588]

-------------------------------------------------------------------------------

threeOneOneLayoutTitle :: String
threeOneOneLayoutTitle = "NYC 311 Complaints"

-------------------------------------------------------------------------------

barChart :: [String] -> String -> [(String, Int)] -> Colour Double -> Renderable ()
barChart titles layoutTitle counts color = toRenderable layout
  where
    x_axis_labels = map fst counts
    y_axis_values = map (\ x -> [snd x]) counts

    barChart =
                  plot_bars_titles  .~ titles
                $ plot_bars_values  .~ addIndexes y_axis_values
                $ plot_bars_style   .~ BarsClustered
                $ plot_bars_spacing .~ BarsFixGap 10 5
                $ plot_bars_item_styles .~ repeat (solidFillStyle $ opaque color, Nothing)
                $ def

    layout =
                layout_title .~ layoutTitle
              $ layout_x_axis . laxis_generate .~ autoIndexAxis x_axis_labels
              $ layout_left_axis_visibility . axis_show_ticks .~ False
              $ layout_plots .~ [plotBars barChart]
              $ def :: Layout PlotIndex Int

-------------------------------------------------------------------------------

chartBoroughsPopSizes :: [ComplaintEntry] -> Renderable ()
chartBoroughsPopSizes complaints = barChart titles layoutTitle counts color
  where
    counts = boroughsPopSizes
    titles = ["Borough Population Size"]
    layoutTitle = "2015 Estimated NYC Borough Population Sizes"
    color  = blueviolet

chartComplaintsCountByBoroughs :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByBoroughs complaints = barChart titles threeOneOneLayoutTitle counts color
  where
    titles = [
          "Complaint Count by Borough from "
        ++ (take 10 $ minCreatedDate complaints)
        ++ " to "
        ++ (take 10 $ maxCreatedDate complaints)
      ]
    counts = complaintsCountByBoroughs complaints
    color  = plum

chartComplaintsCountByBoroughs2015 :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByBoroughs2015 complaints = barChart titles threeOneOneLayoutTitle counts color
  where
    titles = ["Complaint Count by Borough for 2015"]
    counts = complaintsCountByBoroughs2015 complaints
    color  = plum

chartComplaintsCountByYearMonth :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByYearMonth complaints = barChart titles threeOneOneLayoutTitle counts color
  where
    titles = ["Complaint Count per Year and Month"]
    counts = complaintsCountByYearMonth complaints
    color  = turquoise

chartComplaintsCountByYearMonth2015 :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByYearMonth2015 complaints = barChart titles threeOneOneLayoutTitle counts color
  where
    titles = ["Complaint Count per Year and Month for 2015"]
    counts = complaintsCountByYearMonth2015 complaints
    color  = turquoise

-------------------------------------------------------------------------------

lineChart :: (Ord x0, Ord y0, PlotValue x0, PlotValue y0) => [Plot x0 y0] -> Renderable ()
lineChart linePlots = toRenderable layout
  where
    layout =
              layout_title .~ threeOneOneLayoutTitle
            $ layout_x_axis . laxis_override .~ axisGridHide
            $ layout_plots .~ linePlots
            $ layout_grid_last .~ False
            $ def

linePlotAtMonth :: [ComplaintEntry] -> PlotLines LocalTime Int
linePlotAtMonth complaints =
    plot_lines_style .~ (solidLine 3.0 $ opaque skyblue)
  $ plot_lines_values .~ [complaintsCountByCreatedDatesAtMonth complaints]
  $ plot_lines_title .~ "Complaint Count Over Time (Bucketed by Month)"
  $ def

linePlotAtDay :: [ComplaintEntry] -> PlotLines LocalTime Int
linePlotAtDay complaints =
    plot_lines_style .~ (solidLine 3.0 $ opaque tomato)
  $ plot_lines_values .~ [complaintsCountByCreatedDatesAtDay complaints]
  $ plot_lines_title .~ "Complaint Count Over Time (Bucketed by Day)"
  $ def

chartComplaintsCountByCreatedDatesAtMonth :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByCreatedDatesAtMonth complaints = lineChart plots
  where
    plots = [toPlot $ linePlotAtMonth complaints]

chartComplaintsCountByCreatedDatesAtDay :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByCreatedDatesAtDay complaints = lineChart plots
  where
    plots = [toPlot $ linePlotAtDay complaints]

chartComplaintsCountByCreatedDatesAtMonthDay :: [ComplaintEntry] -> Renderable ()
chartComplaintsCountByCreatedDatesAtMonthDay complaints = lineChart plots
  where
    plots = map toPlot [linePlotAtDay complaints, linePlotAtMonth complaints]

-------------------------------------------------------------------------------

chartDaysOfWeekComplaintCounts :: [ComplaintEntry] -> Renderable ()
chartDaysOfWeekComplaintCounts complaints = toRenderable layout
  where
    layout =    layout_title .~ threeOneOneLayoutTitle
              $ layout_plots .~ [toPlot candleChart]
              $ def

    daysOfWeekCounts = complaintsCountsPerDayOfWeek complaints

    makeCandle :: (String, [Int]) -> Candle Int Int
    makeCandle dayOfWeekcounts = Candle xIndex low open med close high
      where
        dayOfWeek   = fst dayOfWeekcounts
        counts      = DL.sort $ snd dayOfWeekcounts
        xIndex      = case DL.elemIndex dayOfWeek daysOfWeek of
                        Nothing -> 0
                        Just i  -> i
        low         = minimum counts

        open        = case median $ take index counts of
                        Nothing -> 0
                        Just m  -> fst m

        medianIndex = case median counts of
                        Nothing -> (0, 0)
                        Just m  -> m
        med         = fst medianIndex
        index       = snd medianIndex

        close       = case median $ drop (index + 1) counts of
                        Nothing -> 0
                        Just m  -> fst m

        high        = maximum counts

    candles = map makeCandle daysOfWeekCounts

    candleChart =   plot_candle_line_style      .~ (    line_width .~ 7
                                                      $ line_color .~ opaque mediumaquamarine
                                                      $ def
                                                   )
                  $ plot_candle_fill            .~ True
                  $ plot_candle_rise_fill_style .~ (fill_color .~ opaque mediumaquamarine $ def)
                  $ plot_candle_tick_length     .~ 1
                  $ plot_candle_width           .~ 10
                  $ plot_candle_values          .~ candles
                  $ plot_candle_title           .~ "Complaint Counts per Day of Week (Sunday = 0)"
                  $ def

-------------------------------------------------------------------------------

complaintsURL :: String
complaintsURL = "https://data.cityofnewyork.us/resource/fhrw-4uyv.json"

complaintsFile :: String
complaintsFile = "./data/complaints.json"

getComplaints :: IO [ComplaintEntry]
getComplaints = do
  fileExists <- doesFileExist complaintsFile
  if fileExists
    then getComplaintsFromFile
    else do
      complaints <- getComplaintsFromURL
      DBSL.writeFile complaintsFile $ encode (DSeq.fromList complaints)
      return complaints

getComplaintsFromFile :: IO [ComplaintEntry]
getComplaintsFromFile = do
  complaintsRaw <- DBSL.readFile complaintsFile
  let complaints =  case decode complaintsRaw :: Maybe [ComplaintEntry] of
                      Nothing -> []
                      Just c  -> c
  return complaints

getComplaintsFromURL :: IO [ComplaintEntry]
getComplaintsFromURL = do
  let opts =  defaults
                & param "$where" .~ ["complaint_type like 'Uri%in Public'"]
                & param "$order" .~ ["created_date DESC"]
                & param "$limit" .~ ["50000"]
  r <- asJSON =<< getWith opts complaintsURL :: IO (Response [ComplaintEntry])
  print $ r ^. responseStatus
  let complaints =  case (r ^? responseBody) of
                      Nothing -> []
                      Just c  -> c
  return complaints

-------------------------------------------------------------------------------

hashableCounts' :: (Hashable k, Ord k) => [k] -> DHM.Map k Int -> DHM.Map k Int
hashableCounts' []    mpIn = mpIn
hashableCounts' (x:y) mpIn = hashableCounts' y mpOut
  where
    value = case DHM.lookup x mpIn of
              Nothing -> 0
              Just v  -> v + 1
    mpOut = DHM.insert x value mpIn

hashableCounts :: (Hashable k, Ord k) => [k] -> DHM.Map k Int
hashableCounts []    = DHM.empty
hashableCounts (x:y) = hashableCounts' (x:y) DHM.empty

data DTFmtTrunc = AtMonth | AtDay | AtSecond deriving (Enum)

dateTimeStringToLocalTime :: DTFmtTrunc -> String -> LocalTime
dateTimeStringToLocalTime trunc s = case trunc of
                                      AtMonth  -> LocalTime fg' midnight
                                      AtDay    -> LocalTime fg  midnight
                                      AtSecond -> LocalTime fg  tod
  where
        --  1234567890123456789
        -- "2016-04-18T19:42:20.000"
        year   = read (take 4 s) :: Integer
        month  = read (take 2 $ drop 5 s) :: Int
        day    = read (take 2 $ drop 8 s) :: Int
        hour   = read (take 2 $ drop 11 s) :: Int
        minute = read (take 2 $ drop 14 s) :: Int
        second = read (take 2 $ drop 17 s) :: Pico
        fg  = fromGregorian year month day
        fg' = fromGregorian year month 01
        tod = TimeOfDay hour minute second

median :: (Ord a) => [a] -> Maybe (a, Int)
median []    =  Nothing
median (x:y) = quickSelect midEven (x:y) >>= (\x -> Just (x, midEven))
  where
    midEven = length (x:y) `div` 2

quickSelect :: (Ord a) => Int -> [a] -> Maybe a
quickSelect _ []        = Nothing
quickSelect n _
  | n < 0               = Nothing
quickSelect n (pivot:y) = case found of
                            True  ->  Just pivot
                            False ->  case goHigher of
                                        True  -> quickSelect (n - (length (pivot:lower))) higher
                                        False -> quickSelect n lower
  where
    lower       = filter (<= pivot) y
    higher      = filter (> pivot) y
    partitioned = lower ++ (pivot:higher)
    found       = partitioned !! n == pivot
    pivotIndex  = case DL.elemIndex pivot partitioned of
                    Nothing -> error "Pivot not found."
                    Just a  -> a
    goHigher    = n > pivotIndex