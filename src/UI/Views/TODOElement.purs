module UI.Views.TODOElement where

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
import UI.Controller.TODOElement

screen :: forall eff. String -> Int -> Screen Action State eff Action
screen todoItem idx = {initialState : (initialState todoItem idx) , view, eval}

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state = linearLayout ([
    height $ V 40,
    width Match_Parent,
    orientation "vertical",
    margin "10,10,10,10"
  ] <> overrides "root" push state)
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
     ]<> overrides "todoLabel" push state)
     ,
     linearLayout ([
       height $ V 27
       , width Match_Parent
       , orientation "horizontal"
       , margin "10, 10, 10, 10"
       ] <> overrides "buttonLinearLayout" push state)
      [
          textView
					([ height $ V 23
					, width $ Match_Parent
					, text "Delete"
					, textSize "18"
					, color "#000000"
					, fontStyle "SourceSansPro-Bold"
					] <> overrides "deleteButton" push state )
      ]
  ]
