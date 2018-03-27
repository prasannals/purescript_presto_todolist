module UI.Views.TODOList where

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
import PrestoDOM.Events (onClick)
import PrestoDOM.Types.Core (PrestoDOM, Screen)
import UI.Controller.TODOList
import UI.Views.TODOElement as TODOElement
import Data.Tuple
import Data.Maybe (fromMaybe, fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Array (length, head, tail, (..), zip)


screen :: forall eff. Screen Action State eff Unit
screen = {initialState, view, eval}

-- zip :: forall a. (Array a) -> Array (Tuple a Int)
-- zip arr = zip' arr 0 []
--
-- zip' :: forall a. (Array a) -> Int -> Array (Tuple a Int)-> Array (Tuple a Int)
-- zip' arr idx acc | (idx >= length arr) = acc
--                 | otherwise = zip' arr (idx + 1) (acc <> [Tuple (fromMaybe)  idx ])
--
-- zip :: forall a b. (Array a) -> (Array b) -> (Array (Tuple a b) )
-- zip arr1 arr2 = zip' arr1 arr2 []
--
-- zip' :: forall a b. (Array a) -> (Array b) -> (Array (Tuple a b)) -> (Array (Tuple a b))
-- zip' arr1 arr2 acc | ((length arr1) == 0) && ((length arr2) == 0) = acc
--                    | otherwise = zip' (fromMaybe [] (tail arr1)) (fromMaybe [] (tail arr2)) (acc <> [Tuple ( unsafeIdx (head arr1) ) (unsafeIdx (head arr2)) ])
--

-- unsafeIdx = unsafePartial $ fromJust

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state = linearLayout [
    height Match_Parent,
    width Match_Parent,
    orientation "vertical",
    margin "10,10,10,10"
  ]
  [
    (linearLayout [
      height $ V 30
      , width Match_Parent
      , orientation "horizontal"
      , margin "10,10,10,10"
    ]
    [
      editText (
        [ height $ V 27
   					, width Match_Parent
   					, textSize "20"
   					, color "#bf000000"
   					, fontStyle "SourceSansPro-Regular"
   					, lineHeight "27px"
   					, gravity "left"
   					, background "#ffffff"
   					, padding "0,0,0,0"
        ]<> overrides "addTodoEdit" push state)
      ,
      textView
      ([ height $ V 23
      , width $ V 30
      , text "Add"
      , textSize "20"
      , color "#000000"
      , fontStyle "SourceSansPro-Bold"
      , margin "10,10,10,10"
      ] <> overrides "addButton" push state )
    ])
    ,

    (linearLayout [
      height Match_Parent
      , width Match_Parent
      , orientation "vertical"
      , margin "10,10,10,10"
    ]
    ([]
    <> map (\(Tuple todo idx) -> (mapDom TODOElement.view push {todo: todo, idx : idx} UpdateElement) ) (zip state.todoList (0..((length state.todoList) - 1)) )
    )
      --(mapDom TODOElement.view push {todo: "First Item", idx : 0} DeleteElement)
    )
  ]
