> -- | Module:    Pipes.Cell
> -- Description: Convertions between cell-based and row-based representations of tabular data
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Pipes converting between cellular and traditional tabular data.

> module Pipes.Cell (
>   toRows, fromRows,
> ) where

> import Control.Monad
> import Data.Cell
> import Pipes

> -- | An infinite pipe that converts a stream of cells into a stream of rows,
> --   with each row represented by a non-empty list of cell values.

> toRows :: (Monad m, Monoid a) => Pipe (Cell a) [a] m ()
> toRows = go [] []
>  where
>   go row cell = do
>     Cell part d <- await
>     let cell' = part:cell
>     case d of
>       EOP -> go row cell'
>       EOC -> go (mconcat (reverse cell') : row) []
>       EOR -> yield (reverse (mconcat (reverse cell') : row)) >> go [] []
>       EOT -> yield (reverse (mconcat (reverse cell') : row)) >> return ()

> -- | An infinite pipe that converts a stream of rows to a stream of cells.

> fromRows :: Monad m => Pipe [a] (Cell a) m r
> fromRows = forever (await >>= fromRow)
>  where
>   fromRow (x:xs) = yield (Cell x (if null xs then EOR else EOC)) >> fromRow xs
>   fromRow [] = return ()

