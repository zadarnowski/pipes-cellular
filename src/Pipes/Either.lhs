> -- | Module:    Pipes.Either
> -- Description: Utilities for conversion between finite and infinite pipes
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable

> module Pipes.Either (
>   catWhileRight, extendWithLeft
> ) where

> import Control.Monad
> import Pipes.Core

> catWhileRight :: Monad m => Pipe (Either r a) b m r
> catWhileRight = go
>  where go = request () >>= either return (respond >=> const go)

> extendEither :: Monad m => Pipe a b m r -> Pipe a (Either r b) m r'
> extendEither p = (p //> respond . Right) >>= forever . respond . Left

