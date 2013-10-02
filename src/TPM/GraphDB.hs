module TPM.GraphDB 
  ( 
    -- * 
    DB,
    DB.new,

    -- * Transactions
    Transaction.Transaction,
    Transaction.Write,
    Transaction.Read,

    -- ** Nodes and Edges
    NodeRef,
    DB.Edge,

    UnionValue,
    IsUnionValueOf(..),

    UnionEdge,
    IsUnionEdgeOf(..),

    -- ** Transaction building blocks
    Transaction.getRoot,
    Transaction.newNode,
    Transaction.getTargets,
    Transaction.getValue,
    Transaction.setValue,
    Transaction.insertEdge,
    Transaction.deleteEdge,

    -- ** Events
    Event.UnionEvent,
    Event.IsUnionEventOf(..),
    Event.Event(..),
    Event.run,
  ) where

import TPM.GraphDB.Prelude
import TPM.GraphDB.DB as DB
import TPM.GraphDB.Transaction as Transaction
import TPM.GraphDB.Transaction.Event as Event
import TPM.GraphDB.Transaction.NodeRef as NodeRef
import TPM.GraphDB.Node as Node
import TPM.GraphDB.Dispatcher as Dispatcher



