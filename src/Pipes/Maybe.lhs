> -- | Module:    Pipes.Maybe
> -- Description: Utilities for conversion between finite and infinite pipes
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable

> module Pipes.Maybe (
>   catMaybe, catWhileJust
> ) where

> import Control.Monad
> import Pipes.Core

> maybe :: Monad m => Pipe a b m r -> Pipe a (Maybe b) m r'
> maybe p = (p //> respond . Just) >> forever (respond Nothing)

> catWhileJust :: Monad m => Pipe (Maybe a) b m ()
> catWhileJust = go
>  where go = await >>= maybe (return ()) (yield >=> const go)

