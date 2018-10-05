module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Math (pow)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as P

data Query a = EnterInitial String a
             | EnterFinal String a

type State = { initial :: Number
             , final :: Number
             , adjusted :: Number }

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { initial: 0.0
                 , final: 0.0
                 , adjusted: 0.0 }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h2_
          [ HH.text "Refractometer Calculator" ]
      , HH.p_
          [ HH.text "Enter your initial and final S.G. readings." ]
      , HH.text "Initial Reading:"
      , HH.input [ HE.onValueInput (HE.input EnterInitial) ]
      , HH.br []
      , HH.text "Final Reading:"
      , HH.input [ HE.onValueInput (HE.input EnterFinal) ]
      , HH.br []
      , HH.text (show state.adjusted)
      ]
  -- FG = 1.0000 – 0.0044993*RIi + 0.011774*RIf + 0.00027581*RIi² – 0.0012717*RIf² – 0.0000072800*RIi³ + 0.000063293*RIf³

  calc :: Number -> Number -> Number
  calc init final =
     1.000 - 0.0044993*init + 0.011774*final + 0.00027581* pow init 2.0 - 0.0012717* pow final 2.0 - 0.0000072800*pow init 3.0 + 0.000063293*pow final 3.0


  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    EnterInitial v next -> do
      _ <- H.modify (\state -> 
            case (fromString v) of
              Just x -> state { initial = x, adjusted = calc x state.final }
              _      -> state)
      pure next

    EnterFinal v next -> do
      _ <- H.modify (\state -> 
            case (fromString v) of
              Just x -> state { final = x, adjusted = calc state.initial x }
              _      -> state)
      pure next
