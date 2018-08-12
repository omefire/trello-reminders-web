module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust, maybe)

import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import React.DOM (text, form, h1', hr', label, b', input, br', select, option, button', button, ul', li') as DOM

import React as React
import ReactDOM as ReactDOM

import React.DOM.Props (action, unsafeMkProps, placeholder, _type, name, required, value, onClick, onChange, title) as Props
import Helpers as H

import Unsafe.Coerce (unsafeCoerce)
import Data.Array (filter)

type Email = String
data SignupPlan = Free | Solo | Team

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
           state: {
              username: "",
              password: "",
              selectedPlanValue: "unselected",
              selectedPlan: Nothing :: Maybe SignupPlan,
              emails: [],
              emailBeingAdded: ""
           },
           render: render <$> React.getState this
         }

    where
    render
       {
         username,
         password,
         selectedPlanValue,
         selectedPlan,
         emails,
         emailBeingAdded
       } =
      
        DOM.form [Props.action "signup.hs"]
        [
          DOM.h1'
          [
            DOM.text "Sign Up"
          ],
          DOM.hr',

          -- Username
          DOM.br',
          DOM.br',
          DOM.label [Props.unsafeMkProps "for" "username"]
          [
            DOM.b'
            [
              DOM.text "Username: "
            ]
          ],
          DOM.input [Props.placeholder "Enter your username", Props._type "text", Props.name "username", Props.required true],

          -- Password
          DOM.br',
          DOM.br',
          DOM.label [Props.unsafeMkProps "for" "password"]
          [
            DOM.b'
            [
              DOM.text "Password: "
            ]
          ],
          DOM.input [Props.placeholder "Enter your password", Props._type "password", Props.name "password", Props.required true],

          -- Choose your plan
          DOM.br',
          DOM.br',
          DOM.label [Props.unsafeMkProps "for" "plan"]
          [
            DOM.b'
            [
              DOM.text "Choose your plan: "
            ]
          ]
          ,
          DOM.select
          [
            Props.name "trello-login",
            Props.required true,
            Props.value selectedPlanValue,
            Props.onChange $ \evt -> do
             {-
                Note: We need this!
                  Otherwise, we get an error during runtime:
                  "Warning: This synthetic event is reused for performance reasons.
                   If you're seeing this, you're accessing the property `target` on a released/nullified synthetic event.
                   This is set to null. If you must keep the original synthetic event around, use event.persist().
                   See https://fb.me/react-event-pooling for more information.

             -}
             let val = (unsafeCoerce evt).target.value <> ""
             React.modifyState this ( \s -> s {
                                         selectedPlanValue = val,
                                         selectedPlan = case selectedPlanValue of                                                   
                                           "free" -> Just Free
                                           "solo" -> Just Solo
                                           "team" -> Just Team
                                           "unselected" -> Nothing
                                           otherwise -> Nothing
                                         }
                                    )
          ]
          [
            DOM.option [Props.value "unselected"] [ DOM.text "Please, select a plan" ],
            DOM.option [Props.value "free"] [ DOM.text "Free (1 person, 5 reminders a week) - For Free" ],
            DOM.option [Props.value "solo"] [ DOM.text "Solo (1 person, unlimited reminders) - $2.99 / month" ],
            DOM.option [Props.value "team"] [ DOM.text "Team (5 people, unlimited reminders) - $9.99 / month" ]
          ],

          -- -- Add/Edit/Delete emails
          DOM.br',
          DOM.br',
          DOM.label [Props.unsafeMkProps "for" "emails"]
          [
            DOM.b'
            [
              DOM.text "Add emails:"
            ]
          ],
          DOM.input
          [
            Props.placeholder "Enter an email address, then press enter",
            Props.value emailBeingAdded,
            Props.onChange $ \evt -> do
              
              {-
                Note: We need this!
                  Otherwise, we get an error during runtime:
                  "Warning: This synthetic event is reused for performance reasons.
                   If you're seeing this, you're accessing the property `target` on a released/nullified synthetic event.
                   This is set to null. If you must keep the original synthetic event around, use event.persist().
                   See https://fb.me/react-event-pooling for more information.

              -}
              let val = (unsafeCoerce evt).target.value <> ""
              React.modifyState this ( \s -> s { emailBeingAdded = val } )
          ],
          
          DOM.button
          [
            Props.onClick (\evt -> do
                              s <- React.getState this
                              let e = s.emailBeingAdded
                              React.modifyState this (\st -> st { emails = emails <> [e], emailBeingAdded = "" } ) )
          ]
          [DOM.text "Add email"],
          
          DOM.ul' $ map (\email -> DOM.li'
                                   [
                                     DOM.text email,
                                     DOM.button
                                     [
                                       Props.onClick \evt -> do
                                          React.modifyState this (\st -> st { emails = filter (\e -> e /= email) emails } )
                                     ]
                                     [DOM.text "Remove"],
                                     DOM.button
                                     [
                                       Props.onClick \evt -> do
                                          H.alert "Replace this by edit code"
                                     ]
                                     [DOM.text "Edit"]
                                   ]
                        ) emails

          -- Payment Details
          
                       
        ]
        
