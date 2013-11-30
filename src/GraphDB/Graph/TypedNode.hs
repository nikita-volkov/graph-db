-- |
-- An API for use-cases, where types are available, e.g. a public API.
module GraphDB.Graph.TypedNode where

import GraphDB.Prelude
import GraphDB.Graph.Types
import qualified GraphDB.Graph.Node as Node; import GraphDB.Graph.Node (Node)
import qualified GraphDB.Graph.DynamicNode as DynamicNode; import GraphDB.Graph.DynamicNode (DynamicNode)

newtype TypedNode t v = TypedNode { node :: Node t }

instance (GraphTag t, IsUnionValue t v) => Serializable IO (TypedNode t v) where
  serialize (TypedNode node) = do
    serialize $ DynamicNode.DynamicNode (toUnionValueType (undefined :: v), node)
  deserialize = do
    DynamicNode.DynamicNode (_, node) <- deserialize
    return $ TypedNode node


new :: v -> IO (TypedNode t v)
new a = TypedNode <$> Node.new (unsafeCoerce a)

-- |
-- TODO: Update indexes!!!
setValue :: v -> TypedNode t v -> IO ()
setValue v (TypedNode node) = Node.setValue (unsafeCoerce v) node

getValue :: TypedNode t v -> IO v
getValue = unsafeCoerce . Node.getValue . node

getTargetsByType :: (Reachable t v v', GraphTag t) => v' -> TypedNode t v -> IO [TypedNode t v']
getTargetsByType v' (TypedNode node) =
  map TypedNode <$> Node.getTargetsByType (toUnionValueType v') node

getTargetsByIndex :: 
  forall t v v'. (Reachable t v v', GraphTag t) => Index t v v' -> TypedNode t v -> IO [TypedNode t v']
getTargetsByIndex i (TypedNode node) =
  map TypedNode <$> Node.getTargetsByTypeAndIndex (toUnionValueType (undefined :: v'), hash i) node

addTarget :: forall t v v'. (Reachable t v v', GraphTag t) => TypedNode t v' -> TypedNode t v -> IO ()
addTarget target source = do
  targetIndexes <- indexes <$> getValue target :: IO [Index t v v']
  Node.addTarget (targetNode, targetUVT, map hash targetIndexes) sourceNode
  where
    targetNode = node target
    targetUVT = toUnionValueType (undefined :: v')
    sourceNode = node source

removeTarget :: forall t v v'. (Reachable t v v', GraphTag t) => TypedNode t v' -> TypedNode t v -> IO ()
removeTarget target source = do
  targetIndexes <- indexes <$> getValue target :: IO [Index t v v']
  Node.removeTarget (targetNode, targetUVT, map hash targetIndexes) sourceNode
  where
    targetNode = node target
    targetUVT = toUnionValueType (undefined :: v')
    sourceNode = node source
