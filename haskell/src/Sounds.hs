module Sounds (main) where

--import Control.Exception (assert)
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
  loadWavFile inpath >>= execMode mode >>= saveWav outpath

data Config = Config { inpath :: FilePath, mode :: Mode, outpath :: FilePath }
data Mode = ThinSampleRate U32 | RepeatWave U32

parseConfig :: [String] -> Config
parseConfig = \case
  [inpath,"halve",outpath] ->
    Config {inpath, mode = ThinSampleRate 2, outpath }
  [inpath,"thin",n, outpath] ->
    Config {inpath, mode = ThinSampleRate (read n), outpath }
  [inpath,"rep",n, outpath] ->
    Config {inpath, mode = RepeatWave (read n), outpath }
  xs ->
    error (printf "unknown args: %s" (show xs))

execMode :: Mode -> Wav -> IO Wav
execMode = \case
  ThinSampleRate n -> thinSampleRate n
  RepeatWave n -> repeatWave n

repeatWave :: U32 -> Wav -> IO Wav
repeatWave n w = do
  let Wav {header,dat} = w
  let Header {numberOfBlocks=nb} = header
  let nb' = nb * n
  printf "Repeat wave %d times (#blocks: %d -> %d)\n" n nb nb'
  let header' = header { numberOfBlocks = nb' }
  let dat' = repeatDat n dat
  pure $ packWav header' dat'

repeatDat :: U32 -> Dat -> Dat
repeatDat n Dat{bs} = do
  let bs' = BS.concat (replicate (cast n) bs)
  Dat {bs = bs'}


thinSampleRate :: U32 -> Wav -> IO Wav
thinSampleRate factor w = do
  let Wav {header,dat} = w
  let Header {sampleRate=r,numberOfBlocks=nb} = header
  let r' = r `div` factor
  let nb' = nb `div` factor
  printf "Thin sample rate by %d: %d --> %d (#blocks: %d -> %d)\n"
    factor r r' nb nb'
  let header' = header { sampleRate = r', numberOfBlocks = nb' }
  let dat' = thinDat factor header dat
  pure $ packWav header' dat'

thinDat :: U32 -> Header -> Dat -> Dat
thinDat factor header Dat{bs} = do
  let Header{numberOfChannels,bitsPerSample,numberOfBlocks} = header
  let bytesPerBlock = cast (numberOfChannels * bitsPerSample `div` 8)
  let bytes = [ BS.index bs (cast i)
              | n <- [0.. numberOfBlocks `div` factor - 1]
              , let off = factor * bytesPerBlock * n
              , i <- [off .. off + bytesPerBlock - 1] ]
  Dat (BS.pack bytes)

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
