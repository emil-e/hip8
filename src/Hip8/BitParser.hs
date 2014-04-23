{-|
Module     : Hip8.BitParser
Maintainer : Emil Eriksson <shadewind@gmail.com>

Parser for parsing bitfields from words
-}

module Hip8.BitParser (
  ParserState,
  maskBits,
  runParser,
  failParse,
  peekBits,
  getBits,
  assertEqBits
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Data.Bits

-- |Parser state containing the data and the remaining number of bits.
data ParserState w = ParserState Int w

-- |The type of a bit parser. Type synonym for a state monad transformer with
-- 'Maybe' as the outer monad type.
type BitParser w a = StateT (ParserState w) Maybe a

-- |Masks the higher bits while retaining the @n@ lower bits.
maskBits :: (Bits a) => Int -> a -> a
maskBits n x | n == bitSize x = x
             | otherwise = maskBits (n + 1) $ clearBit x n

-- |Runs the given parser with the specified data
runParser
  :: BitParser w a -- ^The parser to run
  -> Int           -- ^The number of bits remaining in the data
  -> w             -- ^The data
  -> Maybe a       -- ^The parsed value or 'Nothing' on failure
runParser parser nbits bits = fst <$> runStateT parser (ParserState nbits bits)

-- |Failes the parsing
failParse :: BitParser w a
failParse = lift Nothing

-- |Gets @n@ number of bits from the remaining data without consuming them.
peekBits :: (Bits w) => Int -> BitParser w w
peekBits n = do
  (ParserState nbits bits) <- get
  when (n > nbits) failParse
  return $ maskBits n $ shiftL bits (nbits - n)

-- |Skips @n@ number of bits.
skipBits :: Int -> BitParser w ()
skipBits n = do
  (ParserState nbits bits) <- get
  put $ ParserState (nbits - n) bits

-- |Gets @n@ number of bits from the remaining data without consuming them.
getBits :: (Bits w) => Int -> BitParser w w
getBits n = do
  ret <- peekBits n
  skipBits n
  return ret

-- |Reads the next @n@ bits and checks that they are equal to the given value. If not, the parser
-- fails.
assertEqBits :: (Bits w) => Int -> w -> BitParser w ()
assertEqBits n x = do
  bits <- getBits n
  when (bits /= x) failParse
  
