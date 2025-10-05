-- CaesarCipher.hs
module Main where

import Data.Char (chr, ord, isAlpha, isUpper, isLower, toLower)

-- Изместване на символ с n позиции
shiftChar :: Int -> Char -> Char
shiftChar n c
    | isUpper c = chr $ baseA + ((ord c - baseA + n) `mod` 26)
    | isLower c = chr $ basea + ((ord c - basea + n) `mod` 26)
    | otherwise = c
  where
    baseA = ord 'A'
    basea = ord 'a'

-- Шифриране
caesarEncrypt :: Int -> String -> String
caesarEncrypt n = map (shiftChar n)

-- Дешифриране
caesarDecrypt :: Int -> String -> String
caesarDecrypt n = caesarEncrypt (-n)

-- Главна функция
main :: IO ()
main = do
    putStrLn "Caesar Cipher Tool"
    putStrLn "=================="
    putStrLn "Choose mode: (E)ncrypt or (D)ecrypt?"
    mode <- getLine
    putStrLn "Enter text:"
    text <- getLine
    putStrLn "Enter key (step):"
    keyStr <- getLine
    let key = read keyStr :: Int

    let result = case map toLower mode of
            "e" -> caesarEncrypt key text
            "encrypt" -> caesarEncrypt key text
            "d" -> caesarDecrypt key text
            "decrypt" -> caesarDecrypt key text
            _   -> "Invalid option! Please choose E or D."

    putStrLn "\nResult:"
    putStrLn result
