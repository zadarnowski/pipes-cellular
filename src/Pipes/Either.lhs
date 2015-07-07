> -- | Module:    Pipes.Either
> -- Description: Utilities for conversion between finite and infinite pipes
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Conversion between finite and infinite pipe using the 'Either' type.

> module Pipes.Either (
>   catWhileLeft, catWhileRight,
>   extendWithLeft, extendWithRight,
> ) where

> import Control.Monad
> import Pipes.Core

> -- | A pipe that receives and forwards all 'Left' values in its input
> --   up to the first occurence of a 'Right r' value, returning the value @r@.

> catWhileLeft :: Monad m => Pipe (Either a r) a m r
> catWhileLeft = go
>  where go = request () >>= either (respond >=> const go) return

> -- | A pipe that receives and forwards all 'Right' values in its input
> --   up to the first occurence of a 'Left r' value, returning the value @r@.

> catWhileRight :: Monad m => Pipe (Either r a) a m r
> catWhileRight = go
>  where go = request () >>= either return (respond >=> const go)

> -- | Converts a finite pipe into an infinite pipe, in which the original
> --   pipe's values are followed by an infinite stream of 'Left r'.

> extendWithLeft :: Monad m => Pipe a b m r -> Pipe a (Either r b) m r'
> extendWithLeft p = (p //> respond . Right) >>= forever . respond . Left

> -- | Converts a finite pipe into an infinite pipe, in which the original
> --   pipe's values are followed by an infinite stream of 'Right r'.

> extendWithRight :: Monad m => Pipe a b m r -> Pipe a (Either b r) m r'
> extendWithRight p = (p //> respond . Left) >>= forever . respond . Right


