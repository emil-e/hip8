{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module     : Hip8.BitParser
Maintainer : Emil Eriksson <shadewind@gmail.com>

Parser for parsing bitfields from words
-}

module Hip8.BitParser (
  BitParser,
  maskBits,
  runParser,
  failParse,
  peekBits,
  skipBits,
  getBits,
  getBitsLeft,
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
newtype BitParser w a = BitParser (StateT (ParserState w) Maybe a)
                      deriving (Functor, Applicative, Monad)

instance Alternative (BitParser w) where
  empty = failParse
  (BitParser a) <|> (BitParser b) = BitParser $ StateT $ \s ->
    runStateT a s <|> runStateT b s

-- |Masks the higher bits while retaining the @n@ lower bits.
maskBits :: (Bits a) => Int -> a -> a
maskBits 0 x = complement x .&. x
maskBits n x = x .&. shiftR (complement x .|. x) (bitSize x - n)
  where 

-- |Runs the given parser with the specified data
runParser
  :: Int           -- ^The number of bits remaining in the data
  -> w             -- ^The data
  -> BitParser w a -- ^The parser to run
  -> Maybe a       -- ^The parsed value or 'Nothing' on failure
runParser nbits bits (BitParser parser) = evalStateT parser (ParserState nbits bits)

-- |Returns the state of the parser.
getState :: BitParser w (ParserState w)
getState = BitParser get

-- |Sets the state of the parser.
putState :: ParserState w -> BitParser w ()
putState s = BitParser $ put s

-- |Failes the parsing
failParse :: BitParser w a
failParse = BitParser $ lift Nothing

-- |Gets @n@ number of bits from the remaining data without consuming them.
peekBits :: (Bits w) => Int -> BitParser w w
peekBits n = do
  (ParserState nbits bits) <- getState
  when (n > nbits) failParse
  return $ maskBits n $ shiftR bits (nbits - n)

-- |Skips @n@ number of bits.
skipBits :: Int -> BitParser w ()
skipBits n = do
  (ParserState nbits bits) <- getState
  when (n > nbits) failParse
  putState $ ParserState (nbits - n) bits

-- |Gets @n@ number of bits from the remaining data without consuming them.
getBits :: (Bits w) => Int -> BitParser w w
getBits n = do
  ret <- peekBits n
  skipBits n
  return ret

-- |Gets the number of bits left of the remaining data.
getBitsLeft :: BitParser w Int
getBitsLeft = getState >>= \(ParserState nbits _) -> return nbits

-- |Reads the next @n@ bits and checks that they are equal to the given value. If not, the parser
-- fails.
assertEqBits :: (Bits w) => Int -> w -> BitParser w ()
assertEqBits n x = do
  bits <- getBits n
  when (bits /= x) failParse
  
