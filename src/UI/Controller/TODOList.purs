module UI.Controller.TODOList where

import Prelude
import PrestoDOM.Elements.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Either (Either(..))
import FRP (FRP)
import FRP.Behavior (sample_, step, unfold)
import FRP.Event (create, subscribe)
import Halogen.VDom (buildVDom, extract)
import PrestoDOM.Core (mapDom, getRootNode, insertDom, patchAndRun, spec, storeMachine)
import PrestoDOM.Events (onClick, onChange)
import PrestoDOM.Types.Core (PrestoDOM, Screen)
import UI.Controller.TODOElement (TodoElement)
import Halogen.VDom.DOM.Prop (Prop(..))
import UI.Controller.TODOElement as TODOElement
import Data.Array (take, drop)

-- EditAddress expects the argument to be the index of the address in state
data Action = AddTodo TodoElement | UpdateBuffer TodoElement | DeleteElement TODOElement.Action

type TodoList = Array TodoElement

type State =
  {  todoList :: TodoList
    , addTodoBuffer :: String
  }

initialState :: State
initialState = {todoList : ["1. Get Milk", "2. Prepare Maggi" , "3. Eat" , "4. Sleep"], addTodoBuffer : ""}

eval :: Action -> State -> Either Unit State
eval (AddTodo todo) state = Right $ state {todoList = (state.todoList <> [state.addTodoBuffer]) }
eval (UpdateBuffer todo) state = Right $ state {addTodoBuffer = todo}
eval (DeleteElement (TODOElement.DeleteTodo idx)) state = Right $ state {todoList = (take idx state.todoList) <> (drop (idx + 1) state.todoList) }
eval _ state = Right state

overrides :: forall eff. String -> (Action -> Eff (frp :: FRP | eff) Unit) -> State -> Array (Prop Action)
overrides "addTodoEdit" push state = [name "addTodoEdit", onChange push UpdateBuffer]
overrides "addButton" push state = [name "addButton", onClick push (const $ AddTodo state.addTodoBuffer )]
overrides "todoContainer" push state = []
overrides _ push state = []
