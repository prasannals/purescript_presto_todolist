module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import FRP (FRP)
import PrestoDOM.Core (runScreen)
import PrestoDOM.Types.Core (Screen)
import UI.Views.TODOElement as TODOElement
import UI.Views.TODOList as TODOList

main :: forall eff. Eff ( frp :: FRP, dom :: DOM, console :: CONSOLE, exception :: EXCEPTION | eff ) Unit
main = do
  _ <- launchAff $ runUI $ TODOList.screen
  pure unit
-- runUI :: forall action eff retAction st. Screen action st eff retAction
--          -> Aff (frp :: FRP, dom :: DOM, console :: CONSOLE | eff) Unit
runUI screen = do
  _ <- makeAff (\err sc -> runScreen screen sc)
  log "Completed"
