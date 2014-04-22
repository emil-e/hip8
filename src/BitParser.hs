{-|
Module     : BitParser
Maintainer : Emil Eriksson <shadewind@gmail.com>

Parser for parsing bitfields from words
-}

module BitParser (
  ParserState,
  maskBits,
  failParse,
  peekBits,
  getBits,
  assertEqBits
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad
import Data.Bits

-- |Parser state containing the data and the remaining number of bits.
data ParserState w = ParserState w Int

type BitParser w a = StateT (ParserState w) Maybe a

-- |Masks the higher bits while retaining the @n@ lower bits.
maskBits :: (Bits a) => Int -> a -> a
maskBits n x | n == bitSize x = x
             | otherwise = maskBits (n + 1) $ clearBit x (n + 1)

-- |Failes the parsing
failParse :: BitParser w a
failParse = lift Nothing

-- |Gets @n@ number of bits from the remaining data without consuming them.
peekBits :: (Bits w) => Int -> BitParser w w
peekBits n = do
  (ParserState bits nbits) <- get
  when (n > nbits) failParse
  return $ maskBits n $ shiftL bits (nbits - n)

-- |Skips @n@ number of bits.
skipBits :: Int -> BitParser w ()
skipBits n = do
  (ParserState bits nbits) <- get
  put $ ParserState bits (nbits - n)

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
  
