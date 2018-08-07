module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust)

import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import React.DOM (text, form, h1', hr', label, b', input, br') as DOM

import React as React
import ReactDOM as ReactDOM

import React.DOM.Props (action, unsafeMkProps, placeholder, _type, name, required) as Props

main :: Effect Unit
main = void $ do
  window <- DOM.window
  document <- DOM.document window
  let node = DOM.toNonElementParentNode document
  element <- DOM.getElementById "trello-reminders" node
  let element' = unsafePartial (fromJust element)
  ReactDOM.render (React.createLeafElement signUpClass { }) element'

signUpClass :: React.ReactClass { }
signUpClass = React.component "SignUp" component
  where
  component this =
    pure
         {
           state: {},
           render: render
         }

    where
    render = do
      pure $
        DOM.form
        [Props.action "signup.hs"]
        [
          DOM.h1'
          [
            DOM.text "Sign Up"
          ],
          DOM.hr',

          -- Trello Login
          DOM.label [Props.unsafeMkProps "for" "trello-login"]
          [
            DOM.b'
            [
              DOM.text "Trello Login: "
            ]
          ],
          DOM.input [Props.placeholder "Enter your Trello login", Props._type "text", Props.name "trello-login", Props.required true],

          -- Choose your plan
          DOM.br',
          DOM.label [Props.unsafeMkProps "for" "plan"]
          [
            DOM.b'
            [
              DOM.text "Choose your plan: "
            ]
          ],
          DOM.input [Props.placeholder "Enter your Trello login", Props._type "text", Props.name "trello-login", Props.required true]
        ]
        
  
