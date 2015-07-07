> -- | Module:    Pipes.ByteString.Chunks
> -- Description: Chunked lazy bytestring pipes
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- When binary data is streamed through a pipe, it's often partitioned
> -- arbitrarily into /packets/ with no semantic significance, beyond
> -- facilitation of processing in constant space. This is similar to
> -- the chunking mechanism within lazy bytestrings, and, accordingly,
> -- it's often convenient to unify the two mechanisms, by converting
> -- between pipes over lazy bytestrings and pipes over strict chunks
> -- these bytestrings. This module provides efficient implementations
> -- of these conversions.

> module Pipes.ByteString.Chunks (
>   toChunks, fromChunks
> ) where

> import Data.ByteString (ByteString)
> import Data.Int
> import Pipes

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy
> import qualified Pipes.Prelude as Pipes

> -- | An infinite pipe that converts lazy bytestring
> --   input into strict bytestring chunk output.

> toChunks :: Monad m => Pipe Lazy.ByteString ByteString m r
> toChunks = Pipes.map Lazy.toChunks >-> Pipes.concat

> -- | An infinite pipe that converts strict bytestrings
> --   into lazy bytestrings with the specified minimum
> --   and maximum size, without copying any data.

> fromChunks :: Monad m => Int64 -> Int64 -> Pipe ByteString Lazy.ByteString m r
> fromChunks m n
>   | (m > n || n <= 0) = error ("fromChunks: invalid string size range: [" ++ show m ++ ".." ++ show n ++ "]")
>   | (m <= 0) = fromMaxChunks n
>   | (n >= fromIntegral (maxBound::Int)) = fromMinChunks m
>   | (m == n) = fromExactChunks n
>   | otherwise = loop m n []
>  where
>   loop rm rn cs = do
>     c <- await
>     loop' rm rn cs c (fromIntegral (ByteString.length c))
>   loop' rm rn cs c cl =
>     if cl == 0 then
>       loop rm rn cs -- skip empty chunks
>     else
>       if cl < rm then
>         loop (rm - cl) (rn - cl) (c:cs)
>       else if cl <= rn then
>         yield (Lazy.fromChunks (reverse (c:cs))) >> loop m n []
>       else
>         let (c1, c2) = ByteString.splitAt (fromIntegral rn) c
>         in yield (Lazy.fromChunks (reverse (c1:cs))) >> loop' m n [] c2 (cl - rn)

> fromMaxChunks :: Monad m => Int64 -> Pipe ByteString Lazy.ByteString m r
> fromMaxChunks n
>   | (n <= 0) = error ("Pipes.fromChunks: invalid maximum chunk size: " ++ show n)
>   | (n >= fromIntegral (maxBound::Int)) = Pipes.map Lazy.fromStrict
>   | otherwise = loop
>  where
>   loop = do
>     c <- await
>     loop' c (fromIntegral (ByteString.length c))
>   loop' c cl
>     | (cl <= n) = yield (Lazy.fromStrict c) >> loop
>     | otherwise = let (c1, c2) = ByteString.splitAt n' c in yield (Lazy.fromStrict c1) >> loop' c2 (cl - n)
>   n' = fromIntegral n

> fromMinChunks :: Monad m => Int64 -> Pipe ByteString Lazy.ByteString m r
> fromMinChunks n
>   | (n > 0) = loop n []
>   | otherwise = Pipes.map Lazy.fromStrict
>  where
>   loop r cs = do
>     c <- await
>     loop' r cs c (fromIntegral (ByteString.length c))
>   loop' r cs c cl =
>     if cl == 0 then
>       loop r cs -- skip empty chunks
>     else
>       if cl < r then
>         loop (r - cl) (c:cs)
>       else
>         yield (Lazy.fromChunks (reverse (c:cs))) >> loop 0 []

> fromExactChunks :: Monad m => Int64 -> Pipe ByteString Lazy.ByteString m r
> fromExactChunks n
>   | (n > 0) = loop n []
>   | otherwise = error ("Pipes.fromChunks: invalid chunk size: " ++ show n)
>  where
>   loop r cs = do
>     c <- await
>     loop' r cs c (fromIntegral (ByteString.length c))
>   loop' r cs c cl =
>     if cl == 0 then
>       loop r cs -- skip empty chunks
>     else case compare cl r of
>       LT -> loop (r - cl) (c:cs)
>       EQ -> yield (Lazy.fromChunks (reverse (c:cs))) >> loop 0 []
>       GT -> let (c1, c2) = ByteString.splitAt (fromIntegral r) c in yield (Lazy.fromChunks (reverse (c1:cs))) >> loop' 0 [] c2 (cl - r)
