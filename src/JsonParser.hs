{-# LANGUAGE FlexibleInstances #-}

module JsonParser where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read

class Json a where
    toJson :: a -> JsonValue
    fromJson :: JsonValue -> Maybe a

    encodeJson :: a -> String
    encodeJson jsonObject = show (toJson jsonObject)
    parseJson :: String -> Maybe a
    parseJson jsonString = parse jsonString >>= fromJson

type JsonPair = (String, JsonValue)
data JsonValue = JsonArray [JsonValue]
                    | JsonObject [JsonPair]
                    | JsonString String
                    | JsonInt Int
                    | JsonInteger Integer
                    | JsonDouble Double
                    | JsonBool Bool
                    | JsonNull
                    deriving (Eq)

instance {-# OVERLAPPING #-} Show JsonPair where
    show (string, jsonValue) = "\"" ++ string ++ "\": " ++ show jsonValue

instance Show JsonValue where
    show (JsonString str) = "\"" ++ str ++ "\""
    show (JsonInt int) = show int
    show (JsonInteger int) = show int
    show (JsonDouble double) = show double
    show (JsonBool bool) = show bool
    show JsonNull = "null"
    show (JsonArray list) = "[" ++ intercalate ", " [show e | e <- list] ++ "]"
    show (JsonObject list) = "{" ++ intercalate ", " [show e | e <- list] ++ "}"

parse :: String -> Maybe JsonValue
parse any
    | isFinished parseResult = result
    | otherwise = Nothing
    where
        parseResult = parseNextValue any
        result = fmap fst parseResult

parseNextValue :: String -> Maybe (JsonValue, String)
parseNextValue (char : rest)
    | isSpace char = parseNextValue rest
    | char == '[' = parseNextArray rest
    | char == '{' = parseNextObject rest
    | char == '"' = parseNextString rest
    | isDigit char || char == '-' = parseNextNumber (char : rest)
parseNextValue ('t' : 'r' : 'u' : 'e' : rest) = Just (JsonBool True, rest)
parseNextValue ('f' : 'a' : 'l' : 's' : 'e' : rest) = Just (JsonBool False, rest)
parseNextValue ('n' : 'u' : 'l' : 'l' : rest) = Just (JsonNull, rest)
parseNextValue _ = Nothing

parseNextArray :: String -> Maybe (JsonValue, String)
parseNextArray any = parseNextArrayWithResults any Nothing []

parseNextArrayWithResults :: String -> Maybe JsonValue -> [JsonValue] -> Maybe (JsonValue, String)
parseNextArrayWithResults "" _ _ = Nothing
parseNextArrayWithResults (',' : rest) lastMem fullMem
    = lastMem >>= (\lastValue -> parseNextArrayWithResults rest Nothing (lastValue : fullMem))
parseNextArrayWithResults (']' : rest) Nothing [] = Just (JsonArray [], rest)
parseNextArrayWithResults (']' : rest) lastMem fullMem
    = lastMem >>= (\lastValue -> Just (JsonArray (reverse (lastValue : fullMem)), rest))
parseNextArrayWithResults (char : rest) lastMem fullMem
    | isSpace char = parseNextArrayWithResults rest lastMem fullMem
parseNextArrayWithResults _ (Just _) _ = Nothing
parseNextArrayWithResults json _ fullMem
    = parseNextValue json >>= (\value -> parseNextArrayWithResults (snd value) (Just (fst value)) fullMem)

parseNextObject :: String -> Maybe (JsonValue, String)
parseNextObject any = parseNextObjectWithResults any Nothing []

parseNextObjectWithResults :: String -> Maybe JsonPair -> [JsonPair] -> Maybe (JsonValue, String)
parseNextObjectWithResults "" _ _ = Nothing
parseNextObjectWithResults (',' : rest) lastMem fullMem
    = lastMem >>= (\lastValue -> parseNextObjectWithResults rest Nothing (lastValue : fullMem))
parseNextObjectWithResults ('}' : rest) Nothing [] = Just (JsonObject [], rest)
parseNextObjectWithResults ('}' : rest) lastMem fullMem
    = lastMem >>= (\lastValue -> Just (JsonObject (reverse (lastValue : fullMem)), rest))
parseNextObjectWithResults (char : rest) lastMem fullMem
    | isSpace char = parseNextObjectWithResults rest lastMem fullMem
parseNextObjectWithResults _ (Just _) _ = Nothing
parseNextObjectWithResults json _ fullMem
    = parseNextPair json >>= (\pair -> parseNextObjectWithResults (snd pair) (Just (fst pair)) fullMem)

parseNextPair :: String -> Maybe (JsonPair, String)
parseNextPair (char : rest)
    | isSpace char = Nothing
    | char == '"' =  parseNextString rest >>= (\key -> parseValueInPair (snd key) (extract (fst key)))

parseValueInPair :: String -> String -> Maybe (JsonPair, String)
parseValueInPair "" _ = Nothing
parseValueInPair (':' : rest) key = parseNextValue rest >>= (\value -> Just ((key, (fst value)), (snd value)))

parseNextString :: String -> Maybe (JsonValue, String)
parseNextString any = parseNextStringWithResult any []

parseNextStringWithResult :: String -> String -> Maybe (JsonValue, String)
parseNextStringWithResult "" _ = Nothing
parseNextStringWithResult ('"' : rest) mem = Just (JsonString (reverse mem), rest)
parseNextStringWithResult [char] _ = Nothing
parseNextStringWithResult ('\\' : escaped : rest) mem
    | escaped == '"' = parseNextStringWithResult rest ('"' : mem)
    | escaped == '\\' = parseNextStringWithResult rest ('\\' : mem)
    | escaped == '/' = parseNextStringWithResult rest ('/' : mem)
    | escaped == 'b' = parseNextStringWithResult rest ('\b' : mem)
    | escaped == 'f' = parseNextStringWithResult rest ('\f' : mem)
    | escaped == 'n' = parseNextStringWithResult rest ('\n' : mem)
    | escaped == 'r' = parseNextStringWithResult rest ('\r' : mem)
    | escaped == 't' = parseNextStringWithResult rest ('\t' : mem)
parseNextStringWithResult ('\\' : 'u' : hex1 : hex2 : hex3 : hex4 : rest) mem
    | all isHexDigit hexStr = parseNextStringWithResult rest ((charFromUnicode hexStr) : mem)
    where hexStr = [hex1, hex2, hex3, hex4]
parseNextStringWithResult ('\\' : rest) _ = Nothing
parseNextStringWithResult (char : rest) mem
    | isControl char = Nothing
    | otherwise = parseNextStringWithResult rest (char : mem)

parseNextNumber :: String -> Maybe (JsonValue, String)
parseNextNumber any = parseNextNumberWithResult any [] toInt

parseNextNumberWithResult :: String -> String -> (String -> Maybe JsonValue) -> Maybe (JsonValue, String)
parseNextNumberWithResult "" "" _ = Nothing
parseNextNumberWithResult (char : rest) mem converter
    | isDigit char || char == '-' || char == '+' = parseNextNumberWithResult rest (char : mem) converter
    | char == 'e' || char == '.' = parseNextNumberWithResult rest (char : mem) toDouble
parseNextNumberWithResult remains mem converter = fmap (\m -> (m, remains)) value
    where
        value = fmap reduceInteger (converter (reverse mem))

reduceInteger :: JsonValue -> JsonValue
reduceInteger (JsonInteger int)
    | int <= maxValue && int >= minValue = JsonInt convertedValue
    where
        minValue = toInteger (minBound :: Int)
        maxValue = toInteger (maxBound :: Int)
        convertedValue = fromInteger int :: Int
reduceInteger anyOther = anyOther

charFromUnicode :: String -> Char
charFromUnicode (hex1 : hex2 : hex3 : hex4 : []) = (fst . head . readLitChar) ['\\', 'x', hex1, hex2, hex3, hex4]
charFromUnicode _ = error "Illegal format!"

toInt :: String -> Maybe JsonValue
toInt str = fmap JsonInteger (readMaybe str :: Maybe Integer)

toDouble :: String -> Maybe JsonValue
toDouble str = fmap JsonDouble (readMaybe str :: Maybe Double)

isFinished :: Maybe (JsonValue, String) -> Bool
isFinished Nothing = True
isFinished (Just (_, remains)) = null remains || all isSpace remains

extract :: JsonValue -> String
extract (JsonString string) = string
extract _ = error "Extracting intended for strings only"
