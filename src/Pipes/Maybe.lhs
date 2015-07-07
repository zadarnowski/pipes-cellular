> -- | Module:    Pipes.Maybe
> -- Description: Utilities for conversion between finite and infinite pipes
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Conversion between finite and infinite pipe using the 'Either' type.

> module Pipes.Maybe (
>   catWhileJust, extendWithNothing
> ) where

> import Control.Monad
> import Pipes.Core

> -- | A pipe that receives and forwards all 'Just' values in its input
> --   up to the first occurence of 'Nothing'.

> catWhileJust :: Monad m => Pipe (Maybe a) a m ()
> catWhileJust = go
>  where go = request () >>= maybe (return ()) (respond >=> const go)

> -- | Converts a finite pipe into an infinite pipe, in which the original
> --   pipe's values are followed by an infinite stream of 'Nothing'.

> extendWithNothing :: Monad m => Pipe a b m r -> Pipe a (Maybe b) m r'
> extendWithNothing p = (p //> respond . Just) >> forever (respond Nothing)

