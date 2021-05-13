--Funkcie na prienik a zjednotenie množín v čistom Haskelli. Jedna musela byť napísaná s využitím generátorov zoznamov a druhá iným spôsobom.

inBoth [x] list2 res = res
inBoth (x:xs) list2 res
  | x `elem` list2 = inBoth xs list2 (res ++ [x])
  | otherwise = inBoth xs list2 res

--13b/ Nadefinovat funkciu pt, ktora berie nazov suboru ako argument. Z tohoto suboru nacita zaznamy ve formatu Cislo_typu_Integer#String, pripadne prazdny riadok.
--Zaznam reprezentovat datovym typom DLog. Vypsat zaznamy s cisly, ktere jsou nasobkem 5 (cislo mod 5 == 0). Odelene budu tentoraz dvojbodkou (:).

getLogFromFile :: FilePath -> IO String
getLogFromFile file = readFile file

getId (x:xs) res
 | x == '#' = res
 | otherwise = getId (xs) (res ++ [x])

getRecord line log_id_file = drop (length log_id_file + 1) line

getIds [x] objects = objects ++ [getId x []]
getIds (x:xs) objects = getIds (xs) (objects ++ [getId x []])


getRecords (x:xs) (y:ys) objects = getRecords (xs) (ys) (objects ++ [getRecord x y])


filterList list = filter (/= "") list
data DLog = DLog
  {
    log_id :: Integer,
    value :: String
  } deriving (Eq, Read)

pt file = do
  content <- getLogFromFile file
  let lines_of_file = lines (content)
  let lines_of_file_filtered = filterList lines_of_file
  --let log_id_file = getId lines_of_file []
  --print(lines_of_file)
  return()
