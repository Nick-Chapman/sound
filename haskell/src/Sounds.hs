module Sounds (main) where

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
  [arg1,arg2] <- getArgs
  putStrLn "Example: halving sample rate..."
  pipeline arg1 arg2
  where
    pipeline inpath outpath = do
      w <- loadWavFile inpath
      print (headerWav w)
      let w' = halveSampleRate w
      print (headerWav w')
      saveWav outpath w'

headerWav :: Wav -> Header
headerWav Wav{header=x} = x

halveSampleRate :: Wav -> Wav
halveSampleRate w = do
  let Wav {header,dat} = w
  let Header {sampleRate=r,numberOfSamples=n} = header
  let header' = header { sampleRate = r `div` 2
                       , numberOfSamples = n `div` 2 }
  let dat' = everyOtherSample header dat
  packWav header' dat'

everyOtherSample :: Header -> Dat -> Dat
everyOtherSample header Dat{bs} = do
  let Header{numberOfSamples,bitsPerSample} = header
  let bytesPerSample = fromIntegral bitsPerSample `div` 8
  let bytes = [ BS.index bs (fromIntegral i)
              | n <- [0.. (numberOfSamples`div`2)-1 ]
              , let off = 2 * bytesPerSample * n
              , i <- [off .. off + bytesPerSample - 1] ]
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
packWav header@Header{numberOfSamples,bitsPerSample} dat = do
  let dataSize = numberOfSamples * (fromIntegral bitsPerSample `div` 8)
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
sizeDat Dat{bs} = fromIntegral $ BS.length bs

appendDat :: FilePath -> Dat -> IO ()
appendDat path Dat{bs} = BS.appendFile path bs


data Header = Header
  { numberOfChannels :: U16
  , sampleRate :: U32
  , bitsPerSample :: U16
  , numberOfSamples :: U32
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
  , numberOfSamples } = do
  let dataSize = numberOfSamples * (cast bitsPerSample `div` 8)
  let fileSize = dataSize + (44-8)
  let bytesPerSecond =
        sampleRate * cast bitsPerSample * cast numberOfChannels `div` 8
  let bytesPerBlock =
        numberOfChannels * bitsPerSample `div` 8
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
      cast = fromIntegral
      str :: String -> [U8] = map c2w

      u16 :: U16 -> [U8]
      u16 x = [ fromIntegral (x .&. 0xff)
              , fromIntegral (x `shiftR` 8 .&. 0xff) ]

      u32 :: U32 -> [U8]
      u32 x = [ fromIntegral (x .&. 0xff)
              , fromIntegral (x `shiftR` 8 .&. 0xff)
              , fromIntegral (x `shiftR` 16 .&. 0xff)
              , fromIntegral (x `shiftR` 24 .&. 0xff) ]

headerPar :: Par Header
headerPar = mdo
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
  assertEq "bytesPerSecond" bytesPerSecond $
    sampleRate * cast bitsPerSample * cast numberOfChannels `div` 8
  assertEq "bytesPerBlock" bytesPerBlock $
    numberOfChannels * bitsPerSample `div` 8
  key "data"
  dataSize <- U32
  assertEq "fileSize+8" (fileSize+8) (dataSize+44)
  let numberOfSamples = dataSize `div` (cast bitsPerSample `div` 8)
  pure Header { numberOfChannels, sampleRate, bitsPerSample, numberOfSamples }
  where
    cast = fromIntegral

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
  fromIntegral a + 256 * fromIntegral b

makeLittleEndianU32 :: U8 -> U8 -> U8 -> U8 -> U32
makeLittleEndianU32 a b c d =
  fromIntegral a + 256 * (fromIntegral b + 256 * (fromIntegral c + 256 * fromIntegral d))
