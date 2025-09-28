{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (find)
import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Тип за един ред в таблицата на руните
type RuneRow = (Char, String, String, String)

-- | Таблица със съответствия: (Rune, Latin, AltLatin, Value)
gp :: [RuneRow]
gp =
  [ ('ᚠ',"F","","2")
  , ('ᚢ',"U","V","3")
  , ('ᚦ',"TH","","5")
  , ('ᚩ',"O","","7")
  , ('ᚱ',"R","","11")
  , ('ᚳ',"C","K","13")
  , ('ᚷ',"G","","17")
  , ('ᚹ',"W","","19")
  , ('ᚻ',"H","","23")
  , ('ᚾ',"N","","29")
  , ('ᛁ',"I","","31")
  , ('ᛃ',"J","","37")
  , ('ᛇ',"EO","","41")
  , ('ᛈ',"P","","43")
  , ('ᛉ',"X","","47")
  , ('ᛋ',"S","Z","53")
  , ('ᛏ',"T","","59")
  , ('ᛒ',"B","","61")
  , ('ᛖ',"E","","67")
  , ('ᛗ',"M","","71")
  , ('ᛚ',"L","","73")
  , ('ᛝ',"NG","ING","79")
  , ('ᛟ',"OE","","83")
  , ('ᛞ',"D","","89")
  , ('ᚪ',"A","","97")
  , ('ᚫ',"AE","","101")
  , ('ᚣ',"Y","","103")
  , ('ᛡ',"IA","IO","107")
  , ('ᛠ',"EA","","109")
  ]

-- | Връща индекса на колоната
gpGetColumn :: String -> Int
gpGetColumn t
  | t == "r" = 0
  | t == "l" || t == "t" = 1
  | t == "ll" = 2
  | t == "v" = 3
  | otherwise = -1

-- | Връща стойността от таблицата
gpGet :: Int -> Int -> String
gpGet row col
  | row > length gp - 1 = " "
  | col == -1 = show row
  | otherwise =
      let (a,b,c,d) = gp !! row
      in case col of
           0 -> [a]   -- Char -> String
           1 -> b
           2 -> c
           3 -> d
           _ -> "?"

-- | Търси ред по стойност
gpFind :: String -> Int -> Int
gpFind v col
  | col == -1 = -1
  | otherwise =
      let matches (_, (a,b,c,d)) =
            let value = case col of
                          0 -> [a]
                          1 -> b
                          2 -> c
                          3 -> d
                          _ -> ""
            in value == v
      in case find matches (zip [0..] gp) of
           Just (i,_) -> i
           Nothing -> -1

-- | Замяна на един символ/стойност
gpReplaceSingle :: String -> Int -> Int -> String
gpReplaceSingle v colA colB
  -- Ако колоната не е валидна, оставяме символа непроменен
  | colA == -1 || colB == -1 = v
  | otherwise =
      let r = gpFind v colA
          r' = if r == -1 && colA == 1 then gpFind v 2 else r
      in if r' == -1 then v else gpGet r' colB

-- | Замяна на списък
gpReplaceArray :: [String] -> String -> String -> [String]
gpReplaceArray arr from to =
  let colA = gpGetColumn from
      colB = gpGetColumn to
  in map (\v -> gpReplaceSingle v colA colB) arr

-- | Простое разделяне на символи
gpSplit :: String -> [String]
gpSplit = map (:[])

-- | Сглобяване на списък обратно в низ
gpJoin :: [String] -> String
gpJoin = concat

-- | Предобработка: главни букви
gpPreprocess :: String -> String
gpPreprocess = map toUpper

-- | Главна функция за замяна
gpReplace :: String -> String -> String -> String
gpReplace s from to =
  let s' = gpPreprocess s
      parts = gpSplit s'
      replaced = gpReplaceArray parts from to
  in gpJoin replaced

-- | Wrapper за удобство
translate :: String -> String
translate = (\s -> gpReplace s "l" "r")

-- | Main с безопасен Unicode изход
main :: IO ()
main = do
    let examples = ["Hello", "Welcome", "Cat", "Rune", "Welcome, Pilgrim"]
    mapM_ (\txt -> do
        TIO.putStrLn $ T.pack ("Input: " ++ txt)
        TIO.putStrLn $ T.pack ("Runes: " ++ translate txt)
        TIO.putStrLn "") examples
