--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

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
-- Generate the tag for a rectangle with random color.  Replace the
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
--
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--


mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:g:b:rest)
  | w > 512 && h > 384 = (lr_rest, ul_tags ++ ur_tags ++ ll_tags ++ lr_tags)
  | w > 512 = (r_rest, l_tags ++ r_tags)
  | h > 384 = (b_rest, t_tags ++ b_tags)
  | snd (shouldSplit w rest) && snd (shouldSplit h rest) = (lr_rest, ul_tags ++ ur_tags ++ ll_tags ++ lr_tags)
  | snd (shouldSplit w rest) = (r_rest, l_tags ++ r_tags)
  | snd (shouldSplit h rest) = (b_rest, t_tags ++ b_tags)
  | otherwise = (rectangle x y w h rest)
  -- | otherwise = (rest, rectangle x y w h ((round (r * 255)),
                                          -- (round (g * 255)),
                                          -- (round (b * 255))))
  where
    -- split point
    (w_rest, new_w) = (split w rest)
    (h_rest, new_h) = (split h w_rest)
    -- region split horz and vert
    (ul_rest, ul_tags) = (mondrian x y new_w new_h h_rest)
    (ur_rest, ur_tags) = (mondrian (x + new_w) y (w - new_w) new_h ul_rest)
    (ll_rest, ll_tags) = (mondrian x (y + new_h) new_w (h - new_h) ur_rest)
    (lr_rest, lr_tags) = (mondrian (x + new_w) (y+new_h) (w - new_w) ( h - new_h) ll_rest)
    -- region is split horz
    (l_rest, l_tags) = (mondrian x y new_w h w_rest)
    (r_rest, r_tags) = (mondrian (x + new_w) y (w - new_w) h l_rest)
    -- region is split vert
    (t_rest, t_tags) = (mondrian x y w new_h h_rest)
    (b_rest, b_tags) = (mondrian x (y + new_h) w (h - new_h) t_rest)



split :: Int -> [Float] -> ([Float], Int)
split x (y:rest)
  | y < 0.33 || y > 0.67  = (split x rest)
  | otherwise = (rest, round((fromIntegral x) * y)) -- x `div` (round (y * 10)))

shouldSplit :: Int -> [Float] -> ([Float], Bool)
shouldSplit x (y:rest)
  | x < 120 = (rest, False)  -- if region is smaller than 120 then dont split
  | (y * 1000) < 120 || (y * 1000)  > ((fromIntegral x) * 1.5)  = (shouldSplit x rest)
  | (round (y * 1000)) < x = (rest, True)
  | otherwise = (rest, False)

--
-- Generates an SVG tag for a rectangle.
--
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   (r,g,b) rgb colour value of rectangle
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
rectangle :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
rectangle x y w h (r:rest) = (rest, "<rect x=" ++ (show x) ++
                                    " y=" ++ (show y) ++
                                    " width=" ++ (show w) ++
                                    " height=" ++ (show h) ++
                                    " stroke=\"black\"" ++
                                    " fill=\"rgb(" ++ (determinColour r) ++ ")\" />\n")

determinColour :: Float -> String
determinColour r
  | r < 0.0833  = "255,0,0"
  | r < 0.1667  = "135,206,235"
  | r < 0.25    = "255,255,0"
  | otherwise   = "255,255,255"

--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
