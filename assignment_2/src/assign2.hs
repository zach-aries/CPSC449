--
-- CPSC 449 Assignment 2 Starter Code
--
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

--
-- Define your algebraic types for Part 1 here
--
data QuadTree = Leaf (Int, Int, Int)
              | Node QuadTree QuadTree QuadTree QuadTree
              deriving (Eq, Show)

data QuadTreeImage  = QuadTreeImage Int QuadTree
                    deriving (Show)

--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard.
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int
make32Int a b c d = ((((fromIntegral a) * 256) +
                       (fromIntegral b) * 256) +
                       (fromIntegral c) * 256) +
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block.  Note that this function
-- only handles a single IDAT block, but the PNG standard permits multiple
-- IDAT blocks.
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData [] = error "No IDAT block found"
getImageData (("IDAT", (_:_:_:_:_:_:_:_:xs)) : _) = BS.unpack (Z.decompress (BS.pack (take (length xs - 4) xs)))
getImageData (x:xs) = getImageData xs

--
-- Convert a list of bytes to a list of color tuples
--
makeTuples :: [Word8] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples (x : y : z : vals) = (fromIntegral x, fromIntegral y, fromIntegral z) : makeTuples vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[(Int, Int, Int)]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeTuples (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Convert the entire contents of a PNG file (as a ByteString) into
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[(Int, Int, Int)]]
decodeImage bytes
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | numIDAT blocks > 1 = error "The image contained too many IDAT blocks"
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)

--
-- Helper Functions
--

--
-- Takes a 2D array and returns a sub sub2DArray
--
-- Parameters:
--  x:          2D array
--  rfst, rlst: the first and last index of the rows for the sub sub Array
--  cfst clst:  the first and last index of the columns you want for the sub Array
--  count:      count tracker so you know where which row you are iterating through
--
-- Returns:
--   [[a]]: The new sub Array consiting of i->j rows which consist of n->k columns
--
sub2DArray :: [[a]] -> Int -> Int -> Int -> Int -> Int -> [[a]]
sub2DArray [] _ _ _ _ _ = []
sub2DArray (x:xs) rfst rlst cfst clst count
  | ((count >= rfst) && (count <= rlst)) = [(subArray x cfst clst 0)] ++ (sub2DArray xs rfst rlst cfst clst (count + 1))
  | (count < rfst) = (sub2DArray xs rfst rlst cfst clst (count + 1))
  | otherwise = []

--
-- Takes an array and returns a sub array
--
-- Parameters:
--  x:          the original array
--  cfst clst:  the first and last index of the elements you want for the sub array
--  count:      count tracker so you know where which element you are iterating through
--
-- Returns:
--   [a]: The new sub array consiting of i->j elements
--
subArray :: [a] -> Int -> Int -> Int -> [a]
subArray [] _ _ _ = []
subArray (x:xs) cfst clst count
  | ((count >= cfst) && (count <= clst)) = [x] ++ (subArray xs cfst clst (count + 1))
  | (count < cfst) = (subArray xs cfst clst (count + 1))
  | otherwise = []

--
-- Takes a 2D array of rgb values and checks to see if all values in the array are the same
--
-- Parameters:
--  x:          the 2D array
--
-- Returns:
--   Bool: True if all values are the same, Fales if not
--
isHomogenous :: [[(Int,Int,Int)]] -> Bool
isHomogenous x                                  -- First check to see if all rows are equal, if they arent you can return false
  | (allRowsEqual x) = allColumnsEqual (head x) -- if all rows are equal than more work needs to be done, and each item in every rows
  | otherwise = False                           -- needs to be checked for isHomogenous

--
-- Takes a 2D array of values and checks to see if all values in the array are the same
--
-- Parameters:
--  x:  the 2D array
--
-- Returns:
--   Bool: True if all values are the same, Fales if not
--
allRowsEqual ::Eq a => [[a]] -> Bool
allRowsEqual (x:xs)
  | (xs == []) = True             -- base case, all rows are equal if this point is reached
  | (x /= (head xs)) = False      -- check to see if the first row is equal to the second row, if not then all rows are not equal
  | otherwise = allRowsEqual (xs) -- otherwise first two rows are equal so check the second and third row


--
-- Takes an array of values and checks to see if all values in the array are the same
--
-- Parameters:
--  x:  array
--
-- Returns:
--   Bool: True if all values are the same, Fales if not
--
allColumnsEqual :: Eq a => [a] -> Bool
allColumnsEqual (f:rest)
  | (rest == []) = True               -- base case, all items are equal if this point is reached
  | (f /= (head rest)) = False        -- check to see if the first item is equal to the second item, if not then all rows are not equal
  | otherwise = allColumnsEqual (rest)

--
-- Averages the RGB values of the Leaf Nodes of a QuadTree
-- This results in a greyscale representation of the original quadTree passed
--
-- Parameters:
--   q: the original QuadTree
--
-- Returns:
--   QuadTree: The resulting QuadTree after the function has been applied
--
averageLeafRGB :: QuadTree -> QuadTree
averageLeafRGB (Leaf (r,g,b)) = Leaf (avg,avg,avg) -- set the Leaf value to its rgb representation
  where
    avg = ((r+g+b) `div` 3) -- average the 3 rgb values
averageLeafRGB q = q        -- do not do anything to nodes

--
-- Mirrors the QuadTree over the y axis
-- This results in a mirrored representation of the original quadTree value passed
--
-- Parameters:
--   q: the original QuadTree
--
-- Returns:
--   QuadTree: The resulting QuadTree after the function has been applied
--
mirrorNodes :: QuadTree -> QuadTree
mirrorNodes (Node a b c d) = Node b a d c -- flip the nodes along the y axis  so  NE NW -> NW NE
mirrorNodes q = q -- do nothing with the Leaf values                              SE SW    SW SE
--
-- End Helper Functions
--

--
-- Takes an array of rgb values and turns it into a QuadTreeImage
--
-- Parameters:
--  x:  2D array of RGB values
--
-- Returns:
--   QuadTreeImage
--
createTree :: [[(Int,Int,Int)]] -> QuadTreeImage
createTree x
  | (h /= w) = error "Image provided must be 'square', please provide a different image"
  | isHomogenous x = QuadTreeImage h (createLeaf (head (head x)))   -- if the image is homogenous then just make entire thing one colour
  | otherwise = QuadTreeImage h (createNode x)                      -- otherwise create a new node
  where
    h = length x
    w = length (head x)

--
-- Takes an array of rgb values and turns it into a QuadTree
--
-- Parameters:
--  x:  2D array of RGB values
--
-- Returns:
--   QuadTree
--
createNode :: [[(Int,Int,Int)]] -> QuadTree
createNode x
  | isHomogenous x = createLeaf (head (head x))   -- base case: if array passed is homogenous then we make it a leaf
  | otherwise = Node (createNode nw) (createNode ne) (createNode sw) (createNode se) -- otherwise create a new node and recursively call
  where                 -- split Node into 4 quadrants NW,NE,SW,SE this is represented in each Node of the quad tree
    split = (h `div` 2) -- for squares we can just split in half
    h = length x
    nw = sub2DArray x 0 (split - 1) 0 (split - 1) 0
    ne = sub2DArray x 0 (split - 1) split (h-1) 0
    sw = sub2DArray x split (h-1) 0 (split - 1) 0
    se = sub2DArray x split (h-1) split (h-1) 0

--
-- Takes an rgb value and turns it into a QuadTree Leaf
--
-- Parameters:
--  rgb: tuple (Int,Int,Int) of RGB value
--
-- Returns:
--   QuadTree Leaf
--
createLeaf :: (Int,Int,Int) -> QuadTree
createLeaf rgb = Leaf rgb

--
-- Takes a QuadTreeImage and turns it into a String representation
-- of an HTML SVG image
--
-- Parameters:
--  QuadTreeImage
--
-- Returns:
--  String: HTML SVG value
--
toHTML :: QuadTreeImage -> String
toHTML (QuadTreeImage w t) = prefix ++ (parseTree 0 0 w t) ++ suffix
  where
    prefix = "<svg width=\"" ++ (show w) ++ "\" height=\"" ++ (show w) ++ "\">"
    suffix = "</svg>\n"

-- Takes and x, y, height and QuadTree, parses the representation and either returns and SVG values
-- for the Leafs of the QuadTree
--
-- Parameters:
--  x,y:  x,y for the NW upper left corner of the Node
--  h:    the width/height of the Node
-- Returns:
--  String: SVG value
--
parseTree :: Int -> Int -> Int -> QuadTree -> String
parseTree x y h (Leaf (r,g,b)) = rectangle x y h h (r,g,b)
parseTree x y h (Node nw ne sw se) =  (parseTree x y split nw) ++
                                      (parseTree (x + split) y split ne) ++
                                      (parseTree x (y + split) split sw) ++
                                      (parseTree (x + split) (y + split) split se)
  where
    split = (h `div` 2)

--
-- Generates an SVG tag for a rectangle.
--
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   (r,g,b) rgb colour value of rectangle
--
-- Returns:
--   String: The SVG tags that draw the image
--
rectangle :: Int -> Int -> Int -> Int -> (Int,Int,Int) -> String
rectangle x y w h (r,g,b) = "<rect x=" ++ (show x) ++
                                    " y=" ++ (show y) ++
                                    " width=" ++ (show w) ++
                                    " height=" ++ (show h) ++
                                    " fill=\"rgb(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")\" />\n"

--
-- Applies a function to a Quad treeMap
-- will either apply the function to the Leafs and Nodes
--
-- Parameters:
--   f: The function that will be applies
--   q: The quadTree which the function will be applied to
--
-- Returns:
--   QuadTree: The resulting QuadTree after the function has been applied
--
treeMap :: (QuadTree -> QuadTree) -> QuadTree -> QuadTree
treeMap f (Leaf rgb) = f(Leaf rgb)
treeMap f (Node a b c d) = f( Node (treeMap f a) (treeMap f b) (treeMap f c) (treeMap f d))

--
-- Turns a QuadTreeImage into a greyscale version of the QuadTreeImage
--
-- Parameters:
--   q: the original QuadTreeImage
--
-- Returns:
--   QuadTree: The resulting QuadTreeImage after the function has been applied
--
grayscale :: QuadTreeImage -> QuadTreeImage
grayscale (QuadTreeImage h q) = QuadTreeImage h (treeMap averageLeafRGB q)

--
-- Turns a QuadTreeImage into a mirrored version of the QuadTreeImage
--
-- Parameters:
--   q: the original QuadTreeImage
--
-- Returns:
--   QuadTree: The resulting QuadTreeImage after the function has been applied
--
mirror :: QuadTreeImage -> QuadTreeImage
mirror (QuadTreeImage h q) = QuadTreeImage h (treeMap mirrorNodes q)

--
-- Load a PNG file, convert it to a quad tree, mirror it, grayscale it,
-- and write all three images to an .html file.
--
main :: IO ()
main = do
  putStrLn "Enter the full path of the picture you would like to process:"
  filename <- getLine

  putStrLn ("Proccessing: " ++ filename)
  -- Change the name inside double quotes to load a different file
  -- input <- BS.readFile "TwoSquares.png"
  -- input <- BS.readFile "Test_2x2.png"
  -- input <- BS.readFile "Test_Rectangle_1.png"
  input <- BS.readFile filename
  -- input <- BS.readFile "Test_512x512.png"

  -- image is the list representation of the image stored in the .png file
  let image = decodeImage input

  -- Convert the list representation of the image into a tree representation
  let qtree_image = createTree image

  -- Gray scale the tree representation of the image
  let gs = grayscale qtree_image

  -- Mirror the tree representation of the image
  let mi = mirror qtree_image

  -- Write the original, mirrored and grayscale images to quadtree.html
  let prefix = "<html><head></head><body>\n"
      suffix = "</body></html>"

  writeFile "output/quadtree.html" (prefix ++
                            (toHTML qtree_image) ++
                            suffix ++
                            "<br><br><br>" ++
                            (toHTML gs) ++ "<br><br><br>" ++
                            (toHTML mi) ++ "<br><br><br>")

  putStrLn ("Proccessing Complete!")
