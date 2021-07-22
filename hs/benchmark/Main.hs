module Main where

import           Gauge.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain [
  bgroup "fib"
    [ bench "0" $ whnf fib 0
    , bench "1" $ whnf fib 1
    , bench "2" $ whnf fib 2
    ]
  ]
