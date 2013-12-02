-- |
-- A low-level API over nodes as they get stored in the memory.
-- 
-- In the sake of memory efficiency this is a type-blind API.
module GraphDB.Graph.Node where

import GraphDB.Prelude hiding (Any)
import GraphDB.Graph.Types as Setup
import GHC.Exts (Any)
import qualified GraphDB.IOTable as IOTable; import GraphDB.IOTable (IOTable)
import qualified GraphDB.DIOVector as DIOVector; import GraphDB.DIOVector (DIOVector)
import qualified GraphDB.IOStableNameSet as IOStableNameSet; import GraphDB.IOStableNameSet (IOStableNameSet)


data Node t =
  Node
    {-# UNPACK #-} !(IORef Any)
    -- Target refs
    {-# UNPACK #-} !(IOTable (UnionValueType t) (TargetRefs t))
    -- Source refs
    {-# UNPACK #-} !(SourceRefs t)

-- |
-- ACHTUNG! We're using a final hash integer value instead of indexes here.
-- This may cause different indexes to provide the same hash.
-- Possible solution: use double-hashing with two costant salts.
type TargetRefs t = (IOStableNameSet (Node t), IOTable Int (IOStableNameSet (Node t)))
-- |
-- TODO: Really, it can be simply a list, since there's no need for often lookups on it.
-- Lookups happen only on removal of the node as a target.
-- Also linking to root-node is redundant, so we are protected from this list being huge.
type SourceRefs t = IOStableNameSet (Node t)


new :: Any -> IO (Node t)
new a = Node <$> newIORef a <*> IOTable.new <*> IOStableNameSet.new

-- |
-- TODO: Update indexes!!!
setValue :: Any -> Node t -> IO ()
setValue v (Node ref _ _) = writeIORef ref v

getValue :: Node t -> IO Any
getValue (Node ref _ _) = readIORef ref

getTargetsByType :: (GraphTag t) => UnionValueType t -> Node t -> IO [Node t]
getTargetsByType t (Node _ targetRefs _) =
  IOTable.lookup targetRefs t >>= \case
    Nothing -> return []
    Just (plainRefs, _) -> IOStableNameSet.getList plainRefs

getTargetsByTypeAndIndex :: (GraphTag t) => (UnionValueType t, Int) -> Node t -> IO [Node t]
getTargetsByTypeAndIndex (t, i) (Node _ targetRefs _) =
  IOTable.lookup targetRefs t >>= \case
    Nothing -> return []
    Just (_, indexedRefs) -> IOTable.lookup indexedRefs i >>= \case
      Nothing -> return []
      Just set -> IOStableNameSet.getList set

addTarget :: (GraphTag t) => (Node t, UnionValueType t, [Int]) -> Node t -> IO ()
addTarget (target, targetType, targetIndexes) source = do
  addTargetRefs (target, targetType, targetIndexes) source
  addSourceRef source target

addTargetRefs :: (GraphTag t) => (Node t, UnionValueType t, [Int]) -> Node t -> IO ()
addTargetRefs (target, targetType, targetIndexes) (Node _ ttTable _) = do
  IOTable.lookup ttTable targetType >>= \case
    Nothing -> do
      plainRefs <- IOStableNameSet.new
      IOStableNameSet.insert plainRefs target
      indexedRefs <- IOTable.new
      forM_ targetIndexes $ \i -> do
        set <- IOStableNameSet.new
        IOStableNameSet.insert set target
        IOTable.insert indexedRefs i set
      IOTable.insert ttTable targetType (plainRefs, indexedRefs)
    Just (plainRefs, indexedRefs) -> do
      IOStableNameSet.insert plainRefs target
      forM_ targetIndexes $ \i -> do
        IOTable.lookup indexedRefs i >>= \case
          Nothing -> do
            set <- IOStableNameSet.new
            IOStableNameSet.insert set target
            IOTable.insert indexedRefs i set
          Just set -> do
            IOStableNameSet.insert set target

addSourceRef :: Node t -> Node t -> IO ()
addSourceRef source (Node _ _ sRefs) = do
  IOStableNameSet.insert sRefs source

removeTarget :: (GraphTag t) => (Node t, UnionValueType t, [Int]) -> Node t -> IO ()
removeTarget (target, targetType, targetIndexes) source = do
  removeTargetRefs (target, targetType, targetIndexes) source
  removeSourceRef source target
  maintain target

removeTargetRefs :: (GraphTag t) => (Node t, UnionValueType t, [Int]) -> Node t -> IO ()
removeTargetRefs (target, targetType, targetIndexes) (Node _ ttTable _) =
  IOTable.lookup ttTable targetType >>= \case
    Nothing -> return ()
    Just (plainRefs, indexedRefs) -> do
      IOStableNameSet.delete plainRefs target
      forM_ targetIndexes $ \i -> do
        IOTable.lookup indexedRefs i >>= \case
          Nothing -> return ()
          Just set -> do
            IOStableNameSet.delete set target >>= \case
              True -> return ()
              False -> error "Target ref not found"

removeSourceRef :: Node t -> Node t -> IO ()
removeSourceRef source (Node _ _ sRefs) = do
  IOStableNameSet.delete sRefs source >>= \case
    False -> error "Source ref not found"
    True -> return ()

-- |
-- If after being removed the target node has no edges to it left, 
-- it becomes unreachable. It however will not get garbage-collected
-- if it itself retains edges to other nodes, 
-- because this leaves back-references to it in them. 
-- That's why in this case we must delete all outgoing edges from it manually.
maintain :: Node t -> IO ()
maintain source@(Node _ trTable srSet) = do
  IOStableNameSet.getNull srSet >>= \case
    False -> return ()
    True -> do
      IOTable.forM_ trTable $ \(_, (plainRefs, _)) -> do
        IOStableNameSet.forM_ plainRefs $ \(Node _ _ tsrSet) -> do
          IOStableNameSet.delete tsrSet source >>= \case
            True -> return ()
            False -> error "Back-ref not found"

forMTargets_ :: Node t -> ((UnionValueType t, Node t) -> IO ()) -> IO ()
forMTargets_ node action = foldTargets node () $ \() -> action

foldTargets :: Node t -> z -> (z -> (UnionValueType t, Node t) -> IO z) -> IO z
foldTargets (Node _ trTable _) z f =
  IOTable.foldM trTable z $ \z (t, (plainRefs, _)) ->
    IOStableNameSet.foldM plainRefs z $ \z node -> 
      f z (t, node)

getStats :: Node t -> IO (Int, Int)
getStats n = do
  visitedNodes <- IOStableNameSet.new
  nodesAmount <- newIORef 0
  edgesAmount <- newIORef 0
  let
    visitNode n = do
      modifyIORef nodesAmount succ
      IOStableNameSet.insert visitedNodes n
      forMTargets_ n $ \(_, tn) -> do
        modifyIORef edgesAmount succ
        IOStableNameSet.lookup visitedNodes tn >>= \case
          True -> return ()
          False -> visitNode tn
  visitNode n
  (,) <$> readIORef nodesAmount <*> readIORef edgesAmount
