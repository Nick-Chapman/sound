
-- Discrete Fourier Transform. And the inverse operation.

module Dft (R,C,dft,idft,realPart,plex) where

import Text.Printf (printf)

import Prelude qualified
--import Prelude hiding (exp,sum)
import Prelude (Int,Show(..),zip,length,foldl,fromIntegral,(*),(+),(/),(-),pi,sin,cos)

-- Pick a precision for floating point numbers
type R = Prelude.Double
--type R = Prelude.Float -- less precision; better when exploring

type C = Complex R

-- Define own complex numbers...
data Complex a = Complex { real :: a, imag :: a }

instance Show a => Show (Complex a) where
  show Complex{real=x,imag=y} =
    printf "%s+j%s" (show x) (show y)


-- DFT "Discrete Fourier Transform".
dft :: [C] -> [C]
dft xs = do
  let bigN = length xs
  [ sum [ xn `mult` exp (j `mult` plex (pi * float(-2 * k * n) / float bigN))
        | (n,xn) <- zip [0::Int ..] xs
        ]
    | k <- [0::Int .. bigN-1]
    ]

-- Inverse DFT. Differences from DFT noted.
idft :: [C] -> [C]
idft xs = do
  let bigN = length xs
  [ sum [ xn `mult` exp (j `mult` plex (pi * float(2 -- Loose sign
                                                   * k * n) / float bigN))
        | (n,xn) <- zip [0::Int ..] xs
        ] `mult` plex (1 / float bigN) -- Final division by N
    | k <- [0::Int .. bigN-1]
    ]

float :: Int -> R
float = fromIntegral

-- Getting into and out of the complex domain.

plex :: R -> C
plex real = Complex { real, imag = 0 }

j :: C
j = Complex { real = 0, imag = 1 }

--_magnitude :: C -> R
--_magnitude Complex{real=x,imag=y} = Prelude.sqrt (x*x + y*y)

realPart :: C -> R
realPart Complex{real=x} = x

-- Operations on complex domain numbers.

sum :: [C] -> C
sum = foldl add (plex 0)

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
