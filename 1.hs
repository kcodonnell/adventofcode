import Data.Char

convertToDigits :: (String -> String) -> String -> Integer 
convertToDigits f = read . lastTwo . filter isDigit . f
  where lastTwo xs = head xs : [last xs]

summed :: (String -> String) -> [String] -> Integer
summed f = sum . map (convertToDigits  f)

strToInt :: String -> String
strToInt (xs) = case xs of
    [] -> []
    'o':'n':'e':xs -> '1' : strToInt ('e':xs)
    't':'w':'o':xs -> '2' : strToInt ('o':xs)
    't':'h':'r':'e':'e':xs -> '3': strToInt ('e':xs)
    'f':'o':'u':'r':xs -> '4' : strToInt xs
    'f':'i':'v':'e':xs -> '5' : strToInt ('e':xs)
    's':'i':'x':xs -> '6' : strToInt xs
    's':'e':'v':'e':'n':xs -> '7' : strToInt ('n' : xs)
    'e':'i':'g':'h':'t':xs -> '8' :  strToInt ('t':xs)
    'n':'i':'n':'e':xs -> '9': strToInt ('e': xs)
    x : xs -> x : strToInt xs

main :: IO ()
main = do
    input <- readFile "./data.txt"
    let x = lines input
    let sum1 = summed id x
    let sum2 = summed strToInt x
    putStr ("Part 1: " ++ show sum1 ++ "\n")
    putStr ("Part 2: " ++ show sum2 ++ "\n")



