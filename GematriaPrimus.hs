{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (ord, toUpper)
import Data.List (find)
import Data.Maybe (fromMaybe)

-- Тип за един ред от таблицата gp
type RuneRow = (String, String, String, String)

gp :: [RuneRow]
gp =
  [ ("\x16A0","F","","2")
  , ("\x16A2","U","V","3")
  , ("\x16A6","TH","","5")
  , ("\x16A9","O","","7")
  , ("\x16B1","R","","11")
  , ("\x16B3","C","K","13")
  , ("\x16B7","G","","17")
  , ("\x16B9","W","","19")
  , ("\x16BB","H","","23")
  , ("\x16BE","N","","29")
  , ("\x16C1","I","","31")
  , ("\x16C4","J","","37")
  , ("\x16C7","EO","","41")
  , ("\x16C8","P","","43")
  , ("\x16C9","X","","47")
  , ("\x16CB","S","Z","53")
  , ("\x16CF","T","","59")
  , ("\x16D2","B","","61")
  , ("\x16D6","E","","67")
  , ("\x16D7","M","","71")
  , ("\x16DA","L","","73")
  , ("\x16DD","NG","ING","79")
  , ("\x16DF","OE","","83")
  , ("\x16DE","D","","89")
  , ("\x16AA","A","","97")
  , ("\x16AB","AE","","101")
  , ("\x16A3","Y","","103")
  , ("\x16E1","IA","IO","107")
  , ("\x16E0","EA","","109")
  ]

-- Кое поле да вземем
gpGetColumn :: String -> Int
gpGetColumn "r"  = 0
gpGetColumn "l"  = 1
gpGetColumn "t"  = 1
gpGetColumn "ll" = 2
gpGetColumn "v"  = 3
gpGetColumn _    = -1

-- Вземи стойност от ред и колона
gpGet :: Int -> Int -> String
gpGet row col
  | row > 28   = " "
  | col == -1  = show row
  | otherwise  =
      let (a,b,c,d) = gp !! row
      in [a,b,c,d] !! col

-- Намери ред по стойност
gpFind :: String -> Int -> Int
gpFind v col
  | col == -1  = -1
  | otherwise  =
      case find (\(_, (a,b,c,d)) -> [a,b,c,d] !! col == v) (zip [0..] gp) of
        Just (i,_) -> i
        Nothing    -> -1

-- Една замяна
gpReplaceSingle :: String -> Int -> Int -> String
gpReplaceSingle v colA colB =
  let r = gpFind v colA
      r' = if r == -1 && colA == 1
              then gpFind v 2
              else r
  in if r' == -1
       then "?"
       else gpGet r' colB

-- Замяна на цял списък
gpReplaceArray :: [String] -> String -> String -> [String]
gpReplaceArray arr from to =
  let colA = gpGetColumn from
      colB = gpGetColumn to
  in map (\v -> gpReplaceSingle v colA colB) arr

-- За простота ще разделяме по символи (еднобуквено)
gpSplit :: String -> ([String],[String])
gpSplit s = (map (:[]) s, repeat "") -- само стойности, без формат

gpJoin :: ([String],[String]) -> String -> String
gpJoin (vals,_) _ = concat vals

-- Препроцес
gpPreprocess :: String -> String -> String
gpPreprocess s from
  | from == "t" || from == "l" = map toUpper s
  | otherwise                  = s

gpPostprocess :: String -> String -> String
gpPostprocess s _ = s

-- Главна функция
gpReplace :: String -> String -> String -> String
gpReplace s from to =
  let pre       = gpPreprocess s from
      (vals,fs) = gpSplit pre
      vals'     = gpReplaceArray vals from to
      out       = gpJoin (vals',fs) to
  in gpPostprocess out to

-- Пример
main :: IO ()
main = do
  putStrLn $ gpReplace "CAT" "l" "r"
  putStrLn $ gpReplace "HELLO" "l" "r"
