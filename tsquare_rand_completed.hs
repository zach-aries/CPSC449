--
--  Draw the fractal t-square using random colors
--

import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
--  Return the tag for a square in the middle of a region
--  
--  Parameters:
--    x, y: The upper left corner of the region
--    w, h: The width and height of the region
--    r, g, b: Values between 0 and 1 that indicate the amount of red, green
--             and blue that should be used for the square
--
--  Returns:
--    A string containing a rect tag for the square
--
square :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> String
square x y w h r g b =
  "<rect x=" ++ (show (x + div w 4)) ++
  " y=" ++ (show (y + div h 4)) ++
  " width=" ++ (show (div w 2)) ++
  " height=" ++ (show (div h 2)) ++
  " fill=\"rgb(" ++ show (randomInt 0 255 r) ++ ","
                 ++ show (randomInt 0 255 g) ++ ","
                 ++ show (randomInt 0 255 b) ++ ")\" "
                 ++ "stroke=\"none\" />"

--
--  tsquare: Identify the tags needed to draw the fractal tsquare
--  Parameters:
--    x, y: The top left corner of the current region
--    w, h: The width and height of the region
--    (r:g:b:rest): A list of random values.  The r, g, and b values are used
--                  to color the square drawn at the current level while the
--                  rest of the values are passed to subsequent recursive calls
--
--  Return:
--    A string containing tags that can be rendered within an HTML doc as
--    well as the unused random values and a list of the unused random numbers
--
tsquare :: Int -> Int -> Int -> Int -> [Float] -> (String, [Float])
tsquare x y w h (r:g:b:rest)
  | w >= 16 && h >= 16 = -- draw a square in the middle of the region
                       ((square x y w h r g b) ++
                       -- call myself recursively 4 times
                       ul_tags ++
                       ur_tags ++
                       ll_tags ++
                       lr_tags,
                       -- and also return whatever random values were unused
                       -- after the last recursive call
                       lr_rest)
  | otherwise        = (square x y w h r g b, rest)
  where 
    new_w = w `div` 2
    new_h = h `div` 2
    (ul_tags, ul_rest) = (tsquare x y new_w new_h rest)
    (ur_tags, ur_rest) = (tsquare (x + new_w) y new_w new_h ul_rest)
    (ll_tags, ll_rest) = (tsquare x (y + new_h) new_w new_h ur_rest)
    (lr_tags, lr_rest) = (tsquare (x + new_w) (y + new_h) new_w new_h ll_rest)

main :: IO ()
main = do
  -- randomValues is a list of 20,000 random floating point values between 0 
  -- and 1.
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><svg width=\"512\" height=\"512\">"
  let suffix = "</svg></html>"
  let image_tags = fst (tsquare 0 0 512 512 randomValues)

  writeFile "tsquare.html" (prefix ++ image_tags ++ suffix)
