{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Crc32c
  ( bytes
  , mutableBytes
  , chunks
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Crc32c.Table (table)
import Data.Bits (shiftR, xor)
import Data.Bytes.Chunks (Chunks (ChunksCons, ChunksNil))
import Data.Bytes.Types (Bytes (Bytes), MutableBytes (MutableBytes))
import qualified Data.Primitive.ByteArray as PM
import qualified Data.Primitive.Ptr as PM
import Data.Word (Word32, Word8)

-- | Compute the checksum of a slice of bytes.
bytes :: Word32 -> Bytes -> Word32
bytes !acc0 (Bytes arr off len) =
  let go !acc !ix !end =
        if ix < end
          then go (step acc (PM.indexByteArray arr ix)) (ix + 1) end
          else acc
   in xor 0xFFFFFFFF (go (xor acc0 0xFFFFFFFF) off (off + len))

chunks :: Word32 -> Chunks -> Word32
chunks !acc ChunksNil = acc
chunks !acc (ChunksCons x xs) =
  let !acc' = bytes acc x
   in chunks acc' xs

-- | Compute the checksum of a slice of mutable bytes.
mutableBytes ::
  (PrimMonad m) =>
  Word32 ->
  MutableBytes (PrimState m) ->
  m Word32
{-# INLINEABLE mutableBytes #-}
mutableBytes acc0 (MutableBytes arr off len) = do
  let go !acc !ix !end =
        if ix < end
          then do
            w <- PM.readByteArray arr ix
            go (step acc w) (ix + 1) end
          else pure acc
  r <- go (xor acc0 0xFFFFFFFF) off (off + len)
  pure (xor 0xFFFFFFFF r)

-- This might be revived one day.
--
-- x -- | Compute the checksum of a slice into an array of unsliced byte arrays.
-- x byteArrays :: Word32 -> UnliftedVector ByteArray -> Word32
-- x byteArrays !acc0 (UnliftedVector arr off len) =
-- x   let go !acc !ix !end = if ix < end
-- x         then
-- x           let b = PM.indexUnliftedArray arr ix
-- x            in go (bytes acc (Bytes b 0 (PM.sizeofByteArray b))) (ix + 1) end
-- x         else acc
-- x    in go acc0 off (off + len)
-- x
-- x -- | Compute the checksum of a slice into an mutable array of
-- x -- unsliced byte arrays.
-- x mutableByteArrays :: PrimMonad m
-- x   => Word32
-- x   -> MutableUnliftedVector (PrimState m) ByteArray
-- x   -> m Word32
-- x {-# inlineable mutableByteArrays #-}
-- x mutableByteArrays acc0 (MutableUnliftedVector arr off len) =
-- x   let go !acc !ix !end = if ix < end
-- x         then do
-- x           b <- PM.readUnliftedArray arr ix
-- x           go (bytes acc (Bytes b 0 (PM.sizeofByteArray b))) (ix + 1) end
-- x         else pure acc
-- x    in go acc0 off (off + len)

step :: Word32 -> Word8 -> Word32
step !acc !w =
  xor
    (scramble (xor (fromIntegral @Word32 @Word8 acc) w))
    (shiftR acc 8)

scramble :: Word8 -> Word32
scramble w = PM.indexOffPtr table (fromIntegral @Word8 @Int w)
