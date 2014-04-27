module GraphDB.Util.Par
(
  module Exports,
  parMap_
)
where

import GraphDB.Util.Prelude hiding (get)
import Control.Monad.Par as Exports

parMap_ :: (Traversable t) => (a -> b) -> t a -> Par (t b)
parMap_ f xs = mapM (spawn_ . return . f) xs >>= mapM get

parSequence_ :: (Traversable t) => t (Par a) -> Par (t a)
parSequence_ l = mapM spawn_ l >>= mapM get
