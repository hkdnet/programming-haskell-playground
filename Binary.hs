module Binary where

import           Data.Char

data Bit = Z | O deriving Show

b2i :: Bit -> Int
b2i Z = 0
b2i O = 1
i2b :: Int -> Bit
i2b 0 = Z
i2b 1 = O
i2b _ = error "not 0 or 1"

bin2int :: [Bit] -> Int
bin2int []       = 0
bin2int (b : bs) = b2i b + 2 * bin2int bs

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = i2b (n `mod` 2) : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat Z)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : (chop8 $ drop 8 bits)

encode :: String -> [Bit]
encode = concatMap encodeChar
encodeChar :: Char -> [Bit]
encodeChar c = make8 $ int2bin $ ord c

decode :: [Bit] -> String
decode []   = []
decode bits = map decodeChar $ chop8 bits
decodeChar :: [Bit] -> Char
decodeChar bits = chr $ bin2int bits

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

echo2 :: [Bit] -> [Bit]
echo2 xs = xs ++ xs
