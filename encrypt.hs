module Encrypt where

import Control.Arrow ((***))
import Data.Maybe (fromJust)

codeTable :: [(Char, (Int, Int))]
codeTable = 
  zip "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもや_ゆ_よらりるれろわ___をん" 
  [(x, y)|x <- [1..11], y <- [1..5]]

n2a :: Int -> Maybe Char
n2a n = lookup n $ zip [1..11] ['A'..]

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

encrypt' :: String -> [(Int, Int)]
encrypt' = reverse . map (fromJust . fmap swap . enc)
  where
    enc ch = lookup ch codeTable

-- | encrypt string
--
-- Examples:
--
-- >>> encrypt "せいかつ"
-- "C4A2B1D3"
--
-- >>> encrypt "かてい"
-- "B1D4A2"
--
-- >>> encrypt "ぬかみそ"
-- "E3B7A2C5"
--
-- >>> encrypt "さいほう"
-- "C1E6B1A3"
--
encrypt :: String -> String
encrypt = codeToStr . encrypt'

codeToStr :: [(Int, Int)] -> String
codeToStr = concatMap enc
  where
    enc :: (Int, Int) -> String
    enc = uncurry (:) . (fromJust . n2a *** show)

decrypt' :: [(Int, Int)] -> String
decrypt' = map (fromJust . dec . swap) . reverse
  where
    dec cd = lookup cd $ map swap codeTable

decrypt :: String -> String
decrypt = undefined