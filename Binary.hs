module Binary where

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
