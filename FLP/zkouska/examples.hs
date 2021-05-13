
data Graph v = Graph
  {
    vertices :: [v],
    edges :: [(v,v)]
  } deriving (Eq, Show)

-- Vytvořit funkci, co maže izolované uzly v neorientovaném grafu:
--vertixInEdges :: Eq a => (a, a) -> [a] -> [Bool]
vertixInEdges :: Eq a => [a] -> [([a],[a])] -> [a]
vertixInEdges vertix edges
 | any (== True) (map (isInTuple vertix) edges) = vertix
 | otherwise = []

first :: (a, b) -> a
first (x,y) = x

second :: (a, b) -> b
second (x,y) = y

isInTuple :: Eq a => a -> (a,a) -> Bool
isInTuple vertix (x,y)
 | first (x,y) == vertix || second (x,y) == vertix = True
 | otherwise = False

verticesInEdges :: Eq a => [[a]] -> [([a],[a])] -> [[a]]
verticesInEdges [x] edges = [vertixInEdges x edges]
verticesInEdges (x:xs) edges = [vertixInEdges x edges] ++ verticesInEdges xs edges

removeIsolatedVertices :: [[Char]] -> [([Char], [Char])] -> [[Char]]
removeIsolatedVertices vertix edges = filter (/= "") (verticesInEdges vertix edges)

createGraph :: [v] -> [(v, v)] -> Graph v
createGraph input_vertices input_edges = Graph {vertices = input_vertices, edges = input_edges}

changeGraph :: Graph [Char] -> Graph [Char]
changeGraph (Graph vertices edges) = Graph (removeIsolatedVertices vertices edges) edges

-- Dán typ pro neorientovaný graf, soubor, v něm po řádcích jména uzlů, volný řádek, dvojice jmen oddělených dvojtečkou dává hrany.
--Zkontrolovat funkcí checkUG, zda-li je to jinak korektní graf. Vrátit graf.

getGraphFromFile :: FilePath -> IO String
getGraphFromFile file = readFile file


findVertices :: [[Char]] -> [[Char]] -> [[Char]]
findVertices (x:xs) res
 | x /= "" = findVertices (xs) (res ++ [x])
 | otherwise = res

findEdges :: Foldable t => [a1] -> t a2 -> [a1]
findEdges content file_vertices = drop (length file_vertices + 1) content

makeTuple :: [a] -> ([a], [a])
makeTuple edge = (take 1 edge, drop 2 edge)

main :: IO()
main = do
  let graph_structure = createGraph ["A","K","B"] [("A","B"),("C","D")]
  let changed_graph = changeGraph (graph_structure)
  print(changed_graph)

  file_content <- getGraphFromFile "file"
  let linesOfFiles = lines file_content
  print(linesOfFiles)
  let file_vertices = findVertices linesOfFiles []
  print(file_vertices)
  let file_edges = findEdges linesOfFiles file_vertices
  print(file_edges)
  let file_edges_edited = map makeTuple file_edges
  print(file_edges_edited)

  let graph_structure_file = createGraph file_vertices file_edges_edited
  print(graph_structure_file)




  return()
