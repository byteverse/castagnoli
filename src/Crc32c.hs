{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Crc32c
  ( bytes
  , mutableBytes
  , chunks
    -- * TODO: Get rid of primitive-slice dep
  , byteArrays
  , mutableByteArrays
  ) where

import Crc32c.Table (table)
import Data.Word (Word8,Word32)
import Data.Primitive (ByteArray)
import Data.Bytes.Types (Bytes(Bytes),MutableBytes(MutableBytes))
import Control.Monad.Primitive (PrimState,PrimMonad)
import Data.Bits (shiftR,xor)
import Data.Primitive.Slice (UnliftedVector(UnliftedVector))
import Data.Primitive.Slice (MutableUnliftedVector(MutableUnliftedVector))
import Data.Bytes.Chunks (Chunks(ChunksCons,ChunksNil))
import qualified Data.Primitive.Unlifted.Array as PM
import qualified Data.Primitive.ByteArray as PM
import qualified Data.Primitive.Ptr as PM

-- | Compute the checksum of a slice of bytes.
bytes :: Word32 -> Bytes -> Word32
bytes !acc0 (Bytes arr off len) =
  let go !acc !ix !end = if ix < end
        then go (step acc (PM.indexByteArray arr ix)) (ix + 1) end
        else acc
   in xor 0xFFFFFFFF (go (xor acc0 0xFFFFFFFF) off (off + len))

chunks :: Word32 -> Chunks -> Word32
chunks !acc ChunksNil = acc
chunks !acc (ChunksCons x xs) =
  let !acc' = bytes acc x
   in chunks acc' xs

-- | Compute the checksum of a slice of mutable bytes.
mutableBytes :: PrimMonad m
  => Word32
  -> MutableBytes (PrimState m)
  -> m Word32
{-# inlineable mutableBytes #-}
mutableBytes acc0 (MutableBytes arr off len) = do
  let go !acc !ix !end = if ix < end
        then do
          w <- PM.readByteArray arr ix
          go (step acc w) (ix + 1) end
        else pure acc
  r <- go (xor acc0 0xFFFFFFFF) off (off + len)
  pure (xor 0xFFFFFFFF r)

-- | Compute the checksum of a slice into an array of unsliced byte arrays.
byteArrays :: Word32 -> UnliftedVector ByteArray -> Word32
byteArrays !acc0 (UnliftedVector arr off len) =
  let go !acc !ix !end = if ix < end
        then
          let b = PM.indexUnliftedArray arr ix
           in go (bytes acc (Bytes b 0 (PM.sizeofByteArray b))) (ix + 1) end
        else acc
   in go acc0 off (off + len)

-- | Compute the checksum of a slice into an mutable array of
-- unsliced byte arrays.
mutableByteArrays :: PrimMonad m
  => Word32
  -> MutableUnliftedVector (PrimState m) ByteArray
  -> m Word32
{-# inlineable mutableByteArrays #-}
mutableByteArrays acc0 (MutableUnliftedVector arr off len) =
  let go !acc !ix !end = if ix < end
        then do
          b <- PM.readUnliftedArray arr ix
          go (bytes acc (Bytes b 0 (PM.sizeofByteArray b))) (ix + 1) end
        else pure acc
   in go acc0 off (off + len)

step :: Word32 -> Word8 -> Word32
step !acc !w = xor
  (scramble (xor (fromIntegral @Word32 @Word8 acc) w))
  (shiftR acc 8)

scramble :: Word8 -> Word32
scramble w = PM.indexOffPtr table (fromIntegral @Word8 @Int w)
