import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as C8
import Data.Maybe
import Data.List
import Data.Function
import Data.Ord
import System.IO
import Data.Char
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map, toList, lookup, insertWith, showTree)
import qualified Data.Array as A
import Text.PrettyPrint.Boxes
import qualified DxccData as DXCC

-- ----------------- Log processing code -------------------------

pts line band = foldl (+) 0 $ map weightedZone $ map getZones $ filterByBand band line
zones line band = foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand band line
ctys line band = foldl (+) 0 $ map fst $ map weightedZone' $ uniqCty $ map getZonesDxcc $ filterByBand band line

zonesOld line band = length $ nub $ map getZones $ filterByBand band line
ctysOld line band = length $ map thd $ uniqCty $ map getZonesDxcc $ filterByBand band line

thd (_,_,x) = x

qsByBand line band = length $ nub $ map getCall $ filterByBand band line

filterQsos :: String -> [String]
filterQsos x = filter (isPrefixOf "QSO:") $ lines x

bandList =  ["10","15","20","40","80","160"]
processLogBands logFile = do
        inh <- openFile logFile ReadMode
        inpStr <- hGetContents inh
        putStrLn  $ "Points: " ++ (show $ map ( pts (filterQsos inpStr) ) bandList)
        putStrLn $ "QSOs: " ++ (show $ map ( qsByBand (filterQsos inpStr) ) bandList)
        putStrLn $ "CtyPts: " ++ (show $ map ( ctys (filterQsos inpStr) ) bandList)
        putStrLn $ "Dxcc: " ++ (show $  map ( ctysOld (filterQsos inpStr) ) bandList)
        putStrLn $ "ZonePts: " ++ (show $ map ( zones (filterQsos inpStr) ) bandList)
        putStrLn $ "Zones: " ++ (show $ map ( zonesOld (filterQsos inpStr) ) bandList)
        hClose inh

newPoints x = foldl (+) 0 $ map weightedZone $ map getZones $ filterQsos x
newCtyPts x = foldl (+) 0 $ map ( ctys (filterQsos x) ) bandList
newZonePts x = foldl (+) 0 $ map ( zones (filterQsos x) ) bandList

processLog logFile = 
    do
        inh <- openFile logFile ReadMode
        inpStr <- hGetContents inh
        putStrLn $ "qsoPts" ++ "\t" ++ "zonePts" ++ "\t" ++ "ctyPts" ++ "\t" ++ "score"
        putStrLn $ "----------------------------------------"
        let p = newPoints inpStr
            c = newCtyPts inpStr
            z = newZonePts inpStr
            in
            putStrLn $ show p ++ "\t" ++ show z ++ "\t" ++ show c ++ "\t" ++ show (p * (c + 4*z))
        hClose inh


totalPoints logFile = do
        inh <- openFile logFile ReadMode
        inpStr <- hGetContents inh
        print $ foldl (+) 0 $ map weightedZone $ map getZones $ filter (isPrefixOf "QSO:") $ lines inpStr
        hClose inh

pointsData logFile = do
        inh <- openFile logFile ReadMode
        inpStr <- hGetContents inh
        putStrLn $ unlines $ map show $ map weightedZone $ map getZones $ filter (isPrefixOf "QSO:") $ lines inpStr
        hClose inh

bandPoints band logFile = do
        inh <- openFile logFile ReadMode
        inpStr <- hGetContents inh
        printTable $ groupByN 10 $ map show $ map weightedZone $ map getZones $ filterByBand band $ filter (isPrefixOf "QSO:") $ lines inpStr
        hClose inh

bandCalls band logFile = do
        inh <- openFile logFile ReadMode
        inpStr <- hGetContents inh
        printTable $ groupByN 10 $ map getCall $ filterByBand band $ filter (isPrefixOf "QSO:") $ lines inpStr
        hClose inh

allZones logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			putStrLn $ unlines $ map show $ map weightedZone $ nub $ map getZones $ filter (isPrefixOf "QSO:") $ lines inpStr
			hClose inh

bandZones band logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			putStrLn $ unlines $ map show $ map weightedZone $ nub $ map getZones $ filterByBand band $ filter (isPrefixOf "QSO:") $ lines inpStr
			hClose inh

totalZMByBand logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			print $ foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand "10" $ filter (isPrefixOf "QSO:") $ lines inpStr
			print $ foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand "15" $ filter (isPrefixOf "QSO:") $ lines inpStr
			print $ foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand "20" $ filter (isPrefixOf "QSO:") $ lines inpStr
			print $ foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand "40" $ filter (isPrefixOf "QSO:") $ lines inpStr
			print $ foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand "80" $ filter (isPrefixOf "QSO:") $ lines inpStr
			print $ foldl (+) 0 $ map weightedZone $ nub $ map getZones $ filterByBand "160" $ filter (isPrefixOf "QSO:") $ lines inpStr
			hClose inh

totalZM logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			print $ foldl (+) 0 $ map ( zones (filter (isPrefixOf "QSO:") $ lines inpStr) ) ["10","15","20","40","80","160"]
			hClose inh
			
allDxcc logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			putStrLn $ unlines $ map show $ map weightedZone' $ uniqCty $ map getZonesDxcc $ filter (isPrefixOf "QSO:") $ lines inpStr
			hClose inh

uniqCty xs = nubBy eqThird xs
   where eqThird (x1, y1, z1) (x2, y2, z2) = z1 == z2
   
bandCty band logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			printTable $ groupByN 10 $ map show $ map weightedZone' $ uniqCty $ map getZonesDxcc $ filterByBand band $ filter (isPrefixOf "QSO:") $ lines inpStr
			hClose inh

totalDM logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			print $ foldl (+) 0 $ map ( ctys (filter (isPrefixOf "QSO:") $ lines inpStr) ) ["10","15","20","40","80","160"]
			hClose inh
			
totalDMByBand logFile = do
			inh <- openFile logFile ReadMode
			inpStr <- hGetContents inh
			print $ ctys (filter (isPrefixOf "QSO:") $ lines inpStr) "10"
			print $ ctys (filter (isPrefixOf "QSO:") $ lines inpStr) "15"
			print $ ctys (filter (isPrefixOf "QSO:") $ lines inpStr) "20"
			print $ ctys (filter (isPrefixOf "QSO:") $ lines inpStr) "40"
			print $ ctys (filter (isPrefixOf "QSO:") $ lines inpStr) "80"
			print $ ctys (filter (isPrefixOf "QSO:") $ lines inpStr) "160"
			hClose inh
			
weightedZone' (from, to, cty) = (factorArray A.! (from, to), cty)

getZonesDxcc logLine = (read (getZoneSnt logLine) :: Int, read (getZoneRcvd logLine) :: Int, getDxcc $ getCall logLine)

weightedZone :: (Int, Int) -> Int
weightedZone (from, to) = factorArray A.! (from, to)

getZoneRcvd :: String -> String
getZoneRcvd logLine = trim $ take 2 $ drop 73 logLine

getZoneRcvdInt :: String -> Int
getZoneRcvdInt logLine = read (getZoneRcvd logLine) :: Int

getZoneSnt :: String -> String
getZoneSnt logLine = trim $ take 2 $ drop 48 logLine

getZoneSntInt :: String -> Int
getZoneSntInt logLine = read (getZoneSnt logLine) :: Int

getZones logLine = (read (getZoneSnt logLine) :: Int, read (getZoneRcvd logLine) :: Int) 

-- --------------------- Printing code ------------------------------

groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n l
  | n > 0 = (take n l) : (groupByN n (drop n l))
  | otherwise = error "Negative n"

printTable :: [[String]] -> IO ()
printTable rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

-- --------------------- Band-related code ------------------------------

filterByBand :: String -> [String] -> [String]
filterByBand _ [] = []
filterByBand band (x:xs)	| (getBand (getFreq x) == band) = x:filterByBand band xs
							| otherwise						= filterByBand band xs

getFreq :: String -> Int
getFreq logLine = read (trim $ take 5 $ drop 5 logLine) :: Int

getBand :: Int -> String
getBand freq | (1799 < freq) && (freq < 2000) = "160"
             | (3499 < freq) && (freq < 4000) = "80"
             | (6999 < freq) && (freq < 7500) = "40"
             | (13999 < freq) && (freq < 15000) = "20"
             | (20999 < freq) && (freq < 22000) = "15"
             | (27999 < freq) && (freq < 30000) = "10"
             | otherwise = ""
			 
-- --------------------- Graph code ------------------------------
buildGraph :: Ord a => [(a, a, Int)] -> Map a [(a, Int)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]
			   
dijkstra :: Ord a => a -> Map a [(a, Int)] -> Map a (Int, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else 9999999, Nothing)) 
                | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (fst . (ds !)) q
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e
			  
shortestPath :: Ord a => a -> a -> Map a [(a, Int)] -> [a]
shortestPath from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)
	
pickD (x,(y,z)) = y
tableRow zone graph = show' $ map succ $ map pickD $ toList $ dijkstra zone graph
tableRowList zone graph =  map succ $ map pickD $ toList $ dijkstra zone graph
tableRowZoneList zone = tableRowList zone zoneGraph
tableList = concat $ map tableRowZoneList [1..40]

factorArray = A.listArray ((1,1),(40,40)) tableList

tableRowCQ z = tableRow z zoneGraph

table = putStr $ unlines $ map tableRowCQ [1..40]

show' :: Show a => [a] -> String
show' = intercalate "  " . map show

path x y = shortestPath x y zoneGraph
								 
paths x = [(x, y, path x y) | y <- [1..40]]

-- --------------------- DXCC code ---------------------------------
testDxcc = do
        inh <- openFile "ve7sv.log" ReadMode
        inpStr <- hGetContents inh
        putStrLn $ unlines $ map getDxcc $ map getCall $ filter (isPrefixOf "QSO:") $ lines inpStr
        hClose inh

getCall :: String -> String
getCall logLine = trim $ take 14 $ drop 55 logLine

isSlashNumCall call = (isDigit $ last call) && ((=='/') (call !! (length call - 2)))
isSlashAPM call = (isSuffixOf "/A" call) || (isSuffixOf "/P" call) || (isSuffixOf "/M" call)
isSlashQRP call = isSuffixOf "/QRP" call

adjustSlashNum call = (minimumBy (comparing length) $ wordsWhen isDigit call) ++ [(last call)]
getSlashCty call = minimumBy (comparing length) $ wordsWhen (=='/') call

adjustSlash call 
			| isSlashQRP call 		= take (length call - 4) call
			| isSlashAPM call 		= take (length call - 2) call
			| isSlashNumCall call	= adjustSlashNum call
			| elem '/' call			= getSlashCty call
			| otherwise 			= call

invert m = fromListWith (++) pairs
   where pairs = [(v,[k]) | (k,vs) <- toList m, v<-vs]
   
getExactCty call = Data.Map.lookup call $ invert $ fromList DXCC.exactMapString

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

getSnd (_, x, _) = x

getCty call = T.match DXCC.ctyData $ C8.pack $ adjustSlash call

getDxcc call
		| (isNothing $ getExactCty call) && (isNothing $ getCty call) = "UNKNOWN_DXCC"
		| isNothing $ getExactCty call 	= getSnd $ fromJust $ getCty call
		| otherwise 					= head $ fromJust $ getExactCty call

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- ------------------------- Graph data -------------------------
zoneGraph = buildGraph 
						[(1,2,1),(1,3,1),(1,4,1),(1,19,2),(1,31,2),
						(2,4,1),(2,5,1),(2,40,1),
						(3,4,1),(3,6,1),(3,31,2),
						(4,5,1),(4,6,1),
						(5,8,1),(5,14,2),(5,33,2),(5,40,1),
						(6,7,1),(6,8,1),(6,31,2),
						(7,8,1),(7,9,1),(7,10,1),(7,31,2),
						(8,9,1),(8,33,2),(8,35,2),
						(9,10,1),(9,11,1),(9,35,2),
						(10,11,1),(10,12,1),(10,13,1),(10,31,2),(10,32,2),
						(11,13,1),(11,35,2),(11,36,2),(11,38,2),
						(12,13,1),(12,32,2),
						(13,38,2),
						(14,15,1),(14,16,1),(14,33,1),(14,40,2),
						(15,16,1),(15,20,1),(15,33,1),(15,34,1),
						(16,17,1),(16,20,1),(16,21,1),(16,40,2),
						(17,18,1),(17,21,1),(17,23,1),
						(18,19,1),(18,23,1),
						(19,23,1),(19,24,1),(19,25,1),(19,27,2),(19,31,2),
						(20,21,1),(20,34,1),
						(21,22,1),(21,23,1),(21,34,1),(21,37,1),
						(22,23,1),(22,26,1),(22,28,1),(22,29,2),(22,37,1),(22,39,2),
						(23,24,1),
						(24,25,1),(24,26,1),(24,27,1),
						(25,27,1),
						(26,27,1),(26,28,1),
						(27,28,1),(27,31,2),
						(28,29,1),(28,30,1),(28,31,2),
						(29,30,1),(29,39,2),
						(30,32,1),
						(31,32,2),
						(33,34,1),(33,35,1),
						(34,35,1),(34,36,1),(34,37,1),
						(35,36,1),
						(36,37,1),(36,38,1),
						(37,38,1),(37,39,1),
						(38,39,1)]
