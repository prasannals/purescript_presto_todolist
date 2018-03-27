module UI.Controller.TODOElement where

import Prelude
import PrestoDOM.Elements.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes
import Halogen.VDom.DOM.Prop (Prop(..))

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

-- EditTodo Int and DeleteTodo Int, here, Int is the index
data Action = EditTodo Int String
  | DeleteTodo Int

type TodoElement = String

type State =
  {  todo :: TodoElement,
     idx :: Int
  }

initialState :: String -> Int -> State
initialState todoItem idx =
  {
    todo : todoItem,
    idx : idx
  }

eval :: Action -> State -> Either Action State
eval (EditTodo idx text) state = Left $ EditTodo idx text
eval (DeleteTodo idx) state = Left $ DeleteTodo idx

overrides :: forall eff. String -> (Action -> Eff (frp :: FRP | eff) Unit) -> State -> Array (Prop Action)
overrides "todoLabel" push state = [text state.todo, name "todoLabel", onChange push (EditTodo state.idx)]
overrides "deleteButton" push state = [name "deleteButton", onClick push (const $ DeleteTodo state.idx )]
overrides _ push state = []
