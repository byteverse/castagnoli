import Crc32c (bytes,byteArrays)
import Control.Monad (when)
import Data.Primitive (ByteArray)
import Data.Word (Word8)

import qualified GHC.Exts as Exts

main :: IO ()
main = do
  let sample1 = Exts.fromList
        [0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39]
  let expected1 = 0xe3069283
  let actual1 = bytes 0 sample1
  when (actual1 /= expected1) $ fail $
    "ex1: expected " ++ show expected1 ++ " but got " ++ show actual1
  let sample2 = Exts.fromList
        [ (Exts.fromList [0x31 :: Word8,0x32,0x33]) :: ByteArray
        , (Exts.fromList [0x34 :: Word8,0x35,0x36,0x37,0x38,0x39])
        ]
  let expected2 = 0xe3069283
  let actual2 = byteArrays 0 sample2
  when (actual2 /= expected2) $ fail $
    "ex2: expected " ++ show expected2 ++ " but got " ++ show actual2
  
