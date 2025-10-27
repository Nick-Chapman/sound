
-- Fast Discrete Fourier Transform. And inverse operation.

module Dft (R,C,realPart,plex,fft,ifft) where

import Text.Printf (printf)

import Prelude qualified
import Prelude hiding (exp,sum)
--import Prelude (Int,Show(..),zip,length,foldl,fromIntegral,(*),(+),(/),(-),pi,sin,cos)

-- Pick a precision for floating point numbers
type R = Prelude.Double
--type R = Prelude.Float -- less precision; better when exploring

type C = Complex R

-- Define own complex numbers...
data Complex a = Complex { real :: a, imag :: a }

instance Show a => Show (Complex a) where
  show Complex{real=x,imag=y} =
    printf "%s+j%s" (show x) (show y)


sub :: C -> C -> C
sub a b = a `add` ( plex (-1) `mult` b)

ispow2 :: Int -> Bool
ispow2 x = x == 1 || x `mod` 2 ==0 && ispow2 (x `div` 2)

-- Fast Fourier Transform.
fft :: [C] -> [C]
fft xs = if ispow2 (length xs) then fft' xs else error (show ("fft/not-power2",length xs))

fft' :: [C] -> [C]
fft' = \case
  [x] -> [x]
  xs -> do
    let bigN = length xs
    let ms = [ exp (j `mult` plex (pi * float(-2 * k) / float bigN)) | k <- [0..] ]
    let (evenXs,oddXs) = split xs
    let bigE = fft' evenXs
    let bigO = fft' oddXs
    let pairs = zip bigE [ m `mult` o | (m,o) <- zip ms bigO ]
    [ e `add` mo | (e,mo) <- pairs ] ++ [ e `sub` mo | (e,mo) <- pairs ]

split :: [C] -> ([C],[C])
split = \case
  [] -> ([],[])
  x:xs -> let (ys,zs) = split xs in (x:zs,ys)

-- Inverse Fast Fourier Transform. Differences from FFT noted.
ifft :: [C] -> [C]
ifft xs =
  if not (ispow2 (length xs)) then error (show ("ifft/not-power2",length xs)) else do
    let bigN = length xs
    let scale = plex (1 / float bigN)
    [ scale `mult` c | c <- ifft' xs ] -- Final division by N

ifft' :: [C] -> [C]
ifft' = \case
  [x] -> [x]
  xs -> do
    let bigN = length xs
    let ms = [ exp (j `mult` plex (pi * float(2 -- Loose sign
                                              * k) / float bigN)) | k <- [0..] ]
    let (evenXs,oddXs) = split xs
    let bigE = ifft' evenXs
    let bigO = ifft' oddXs
    let pairs = zip bigE [ m `mult` o | (m,o) <- zip ms bigO ]
    [ e `add` mo | (e,mo) <- pairs ] ++ [ e `sub` mo | (e,mo) <- pairs ]


float :: Int -> R
float = fromIntegral

-- Getting into and out of the complex domain.

plex :: R -> C
plex real = Complex { real, imag = 0 }

j :: C
j = Complex { real = 0, imag = 1 }

--mag :: C -> R
--mag Complex{real=x,imag=y} = Prelude.sqrt (x*x + y*y)

realPart :: C -> R
realPart Complex{real=x} = x

-- Operations on complex domain numbers.

exp :: C -> C
exp Complex{real=x,imag=y} =
  Complex { real = m * cos y
          , imag = m * sin y }
  where
    m = Prelude.exp x -- e^x for reals

add :: C -> C -> C
add Complex{real=x1,imag=y1} Complex{real=x2,imag=y2} =
  Complex { real = x1+x2
          , imag = y1+y2 }

mult :: C -> C -> C
mult Complex{real=x1,imag=y1} Complex{real=x2,imag=y2} =
  Complex { real = x1*x2 - y1*y2
          , imag = x1*y2 + x2*y1 }
