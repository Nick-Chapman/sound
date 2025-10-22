module Sounds (main) where

import Control.Exception (assert)
import Control.Monad (ap,liftM)
import Data.Bits ((.&.),shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c,c2w)
import Data.Char qualified as Char
import Data.Word qualified as Word
import System.Environment (getArgs)
import Text.Printf (printf)

type U32 = Word.Word32
type U16 = Word.Word16
type U8 = Word.Word8

main :: IO ()
main = do
  args <- getArgs
  let Config{inpath,outpath,mode} = parseConfig args
  loadWavFile inpath >>= execModeInfo mode >>= saveWav outpath

data Config = Config { inpath :: FilePath, mode :: Mode, outpath :: FilePath }

parseConfig :: [String] -> Config
parseConfig = \case
  [i,"begin"    ,o] -> mk i o $ Begin
  [i,"thin",n   ,o] -> mk i o $ Thin (read n)
  [i,"hr"       ,o] -> mk i o $ Thin 2
  [i,"rep",n    ,o] -> mk i o $ Repeat (read n)
  [i,"mono"     ,o] -> mk i o $ Mono
  [i,"8bit"     ,o] -> mk i o $ EightBit
  [i,"hurry",n  ,o] -> mk i o $ Hurry (read n)
  [i,"vol",f    ,o] -> mk i o $ Dally (read f)
  [i,"dally",n  ,o] -> mk i o $ Dally (read n)
  xs ->
    error (printf "unknown args: %s" (show xs))
  where
    mk inpath outpath mode =
      Config { inpath, mode, outpath }

data Mode
  = Begin
  | Thin U32
  | Repeat U32
  | Mono
  | EightBit
  | Volume Float
  | Hurry U32
  | Dally U32
  deriving Show

execModeInfo :: Mode -> Wav -> IO Wav
execModeInfo mode w = do
  let w'@Wav{header} = execMode mode w
  printf "%s\n" (info (show mode) header)
  pure w'

execMode :: Mode -> Wav -> Wav
execMode = \case
  Begin -> id
  Repeat n -> repeatWave n
  Thin n -> thinSampleRate n
  Mono -> monoize
  EightBit -> eightBit
  Volume{} -> undefined
  Hurry{} -> undefined
  Dally{} -> undefined


repeatWave :: U32 -> Wav -> Wav
repeatWave n w = do
  let Wav {header,dat} = w
  let Header {numberOfBlocks=nb} = header
  let nb' = nb * n
  let header' = header { numberOfBlocks = nb' }
  packWav header' (repeatDat n dat)
  where
    repeatDat :: U32 -> Dat -> Dat
    repeatDat n Dat{bs} = Dat $ BS.concat (replicate (cast n) bs)

thinSampleRate :: U32 -> Wav -> Wav
thinSampleRate factor w = do
  let Wav {header,dat} = w
  let Header {sampleRate=r,numberOfBlocks=nb} = header
  let r' = r `div` factor
  let nb' = nb `div` factor
  let header' = header { sampleRate = r', numberOfBlocks = nb' }
  packWav header' (thinDat factor header dat)

thinDat :: U32 -> Header -> Dat -> Dat
thinDat factor header Dat{bs} = Dat $ do
  let Header{numberOfChannels,bitsPerSample,numberOfBlocks} = header
  let bytesPerBlock = cast (numberOfChannels * bitsPerSample `div` 8)
  BS.pack [ BS.index bs (cast i)
          | n <- [0.. numberOfBlocks `div` factor - 1]
          , let off = factor * bytesPerBlock * n
          , i <- [off .. off + bytesPerBlock - 1] ]

monoize :: Wav -> Wav
monoize w = do
  let Wav {header,dat} = w
  let Header {numberOfChannels,numberOfBlocks} = header
  -- The header we want to end up with.
  let header' = header { numberOfChannels = 1 }
  -- Pretend the current data is already mono, but with a multiplied #blocks.
  let hhack = header' { numberOfBlocks = numberOfBlocks * cast numberOfChannels }
  -- Then we can re-use thinDat
  packWav header' (thinDat (cast numberOfChannels) hhack dat)

eightBit :: Wav -> Wav
eightBit w = do
  let Wav {header,dat} = w
  let Header {bitsPerSample} = header
  assert  (bitsPerSample == 16) $ do
  -- We still write out the Wav using 16bit signed data
  -- but we zeroed all the lo-order byes
  -- To do this, we dont need to know how many channels we have
  packWav header (eightBitDat dat)
  where
    eightBitDat :: Dat -> Dat
    eightBitDat  Dat{bs} = Dat $ do
      BS.pack [ byte
              | i <- [0.. BS.length bs `div` 2  - 1 ]
              , byte <- [ 0, BS.index bs (1 + 2 * cast i) ]
              ]


data Wav = Wav { header :: Header, dat :: Dat }

loadWavFile :: FilePath -> IO Wav
loadWavFile path = makeWav <$> BS.readFile path

makeWav :: ByteString -> Wav
makeWav bs = packWav header dat
  where
    header = execP headerPar (BS.unpack $ BS.take 44 bs)
    dat = makeDat (BS.drop 44 bs)

packWav :: Header -> Dat -> Wav
packWav header@Header{bitsPerSample,numberOfChannels,numberOfBlocks} dat = do
  let bytesPerBlock = cast (numberOfChannels * bitsPerSample `div` 8)
  let dataSize = numberOfBlocks * bytesPerBlock
  let z = sizeDat dat
  if (dataSize==z)
  then Wav { header, dat }
  else error (printf "packWav, dataSize=%d, length(dat)=%d" dataSize z)

saveWav :: FilePath ->  Wav -> IO ()
saveWav path Wav{header,dat} = do
  writeHeader path header
  appendDat path dat


data Dat = Dat { bs :: ByteString }

makeDat :: ByteString -> Dat
makeDat bs = Dat { bs }

sizeDat :: Dat -> U32
sizeDat Dat{bs} = cast $ BS.length bs

appendDat :: FilePath -> Dat -> IO ()
appendDat path Dat{bs} = BS.appendFile path bs


data Header = Header
  { numberOfChannels :: U16
  , sampleRate :: U32
  , bitsPerSample :: U16
  , numberOfBlocks :: U32
  } deriving Show


info :: String -> Header -> String
info tag Header{ numberOfChannels, sampleRate, bitsPerSample, numberOfBlocks} = do
  let duration :: Float = cast numberOfBlocks / cast sampleRate
  printf "%s: %s %dbit  %6dHz  %0.2fs"
    (justify 16 tag)
    (justify 6 (case numberOfChannels of
                  1 -> "mono"
                  2 -> "stereo"
                  n -> printf "%d-chan" n))
    bitsPerSample
    sampleRate
    duration
  where justify n s = s ++ replicate (n - length s) ' '


writeHeader :: FilePath -> Header -> IO ()
writeHeader path header = do
  let bs = byteStringOfHeader header
  if (BS.length bs /= 44) then error "writeHeader: bad #bytes" else
    BS.writeFile path bs

byteStringOfHeader :: Header -> ByteString
byteStringOfHeader Header
  { numberOfChannels
  , sampleRate
  , bitsPerSample
  , numberOfBlocks
  } = do
  let bytesPerBlock = numberOfChannels * bitsPerSample `div` 8
  let bytesPerSecond = sampleRate * cast bytesPerBlock
  let dataSize = numberOfBlocks * cast bytesPerBlock
  let fileSize = dataSize + (44-8)
  BS.pack $ concat
    [ str "RIFF"
    , u32 fileSize
    , str "WAVE"
    , str "fmt "
    , u32 16
    , u16 1
    , u16 numberOfChannels
    , u32 sampleRate
    , u32 bytesPerSecond
    , u16 bytesPerBlock
    , u16 bitsPerSample
    , str "data"
    , u32 dataSize
    ]
    where
      str :: String -> [U8] = map c2w

      u16 :: U16 -> [U8]
      u16 x = [ cast (x .&. 0xff)
              , cast (x `shiftR` 8 .&. 0xff) ]

      u32 :: U32 -> [U8]
      u32 x = [ cast (x .&. 0xff)
              , cast (x `shiftR` 8 .&. 0xff)
              , cast (x `shiftR` 16 .&. 0xff)
              , cast (x `shiftR` 24 .&. 0xff) ]

headerPar :: Par Header
headerPar = do
  key "RIFF"
  fileSize <- U32
  key "WAVE"
  key "fmt "
  U32 >>= assertEq "sixteen" 16
  U16 >>= \case
    1 -> pure ()
    n -> Fail (printf "unexpected format type: %d" n)
  numberOfChannels <- U16
  sampleRate <- U32
  bytesPerSecond <- U32
  bytesPerBlock <- U16
  bitsPerSample <- U16
  assertEq "bytesPerBlock" bytesPerBlock $
    numberOfChannels * bitsPerSample `div` 8
  assertEq "bytesPerSecond" bytesPerSecond $
    sampleRate * cast bytesPerBlock
  key "data"
  dataSize <- U32
  assertEq "fileSize+8" (fileSize+8) (dataSize+44)
  let numberOfBlocks = dataSize `div` cast bytesPerBlock
  pure Header { numberOfChannels, sampleRate, bitsPerSample, numberOfBlocks }

key :: String -> Par ()
key xs = sequence_ [ expectChar x | x <- xs ]

expectChar :: Char -> Par ()
expectChar expected = do
  got <- Char
  if got == expected then pure () else
    Fail $ printf "expected char '%c' (0x%02x) got '%c' (0x%02x)"
    expected (Char.ord expected)
    got (Char.ord got)

assertEq :: (Eq a, Show a) => String -> a -> a -> Par ()
assertEq tag expected actual = if actual == expected then pure () else Fail $
  printf "%s : (actual) %s != (expected) %s" tag (show actual) (show expected)


instance Monad Par where (>>=) = Bind
instance Applicative Par where pure = Pure; (<*>) = ap
instance Functor Par where fmap = liftM

data Par a where
  Pure :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Char :: Par Char
  U16 :: Par U16
  U32 :: Par U32
  Fail :: String -> Par a

type ParState = ([U8], Int)

execP :: Par a -> [U8] -> a
execP par bytes = loop (bytes,0) par kFinal
  where
    kFinal :: ParState-> a -> a
    kFinal = \case
      ([],_) -> \a -> a
      (_:_,i) -> error (printf "unexpected bytes remain at position %d" i)

    loop :: ParState-> Par a -> (ParState -> a -> b) -> b
    loop s par k = case par of
      Pure a -> k s a
      Bind m f -> loop s m $ \s a -> loop s (f a) k
      Char -> do
        next s $ \s b -> k s (charOfByte b)
      U16 -> do
        next s $ \s b1 -> do
          next s $ \s b2 -> do
            k s (makeLittleEndianU16 b1 b2)
      U32 -> do
        next s $ \s b1 -> do
          next s $ \s b2 -> do
            next s $ \s b3 -> do
              next s $ \s b4 -> do
                k s (makeLittleEndianU32 b1 b2 b3 b4)
      Fail mes -> do
        error (printf "bytes parser failed at position %d: %s" (snd s) mes)

    next :: ParState -> (ParState -> U8 -> a) -> a
    next s k = case s of
      ([],i) ->
        error (printf "run out of bytes at position %d" i)
      (b:bs,i) ->
        k (bs,i+1) b

charOfByte :: U8 -> Char
charOfByte = w2c

makeLittleEndianU16 :: U8 -> U8 -> U16
makeLittleEndianU16 a b =
  cast a + 256 * cast b

makeLittleEndianU32 :: U8 -> U8 -> U8 -> U8 -> U32
makeLittleEndianU32 a b c d =
  cast a + 256 * (cast b + 256 * (cast c + 256 * cast d))

cast :: (Integral a, Num b) => a -> b
cast = fromIntegral -- shorter spelling
