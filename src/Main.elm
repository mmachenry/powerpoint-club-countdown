module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, second)

main = Html.program {
  init = (initModel, Cmd.none),
  view = view,
  update = update,
  subscriptions = subscriptions
  }

type alias Model = {
  elapsed : Int,
  maxSeconds : Int,
  countdownActive : Bool
  }

type Msg =
    Tick Time
  | Play
  | Stop

type Color = Green | Red

initModel = {
  elapsed  = 0,
  maxSeconds = 300,
  countdownActive = False
  }

view : Model -> Html Msg
view model = div [] [
  -- text (toString model),
  controls model,
  lights model,
  summary model
  ]

controls : Model -> Html Msg
controls model = div [] [
    button [ onClick Play ] [ text "Play" ],
    button [ onClick Stop ] [ text "Stop" ] ]

lights : Model -> Html Msg
lights model =
  let colors = [Green, Green, Green, Red, Red]
      n = List.length colors
      times = List.map (\i->model.maxSeconds // n * i) (List.range 1 n)
  in div [style [("display","flex")] ]
         (List.map2 (timedDiv model.elapsed) times colors)


timedDiv : Int -> Int -> Color -> Html Msg
timedDiv elapsed delay color =
  let hc = hexCode color (elapsed < delay)
  in div [ style [("background-color", hc),
                  ("color", hc),
                  ("margin", "10px"),
                  ("width", "100px"),
                  ("height", "100px") ] ]
         [ text "-" ]

hexCode : Color -> Bool -> String
hexCode color done = case color of
  Green -> if done then "#D1F2EB" else "#2ECC71"
  Red -> if done then "#F5B7B1" else "#E74C3C"

summary model =
  div [ style [("display", if model.elapsed >= model.maxSeconds
                           then "block"
                           else "none")] ]
      [ text "You're going to die alone in an insane asylum." ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model = (updateModel msg model, Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = case msg of
  Tick _ -> if model.countdownActive
            then let new = model.elapsed + 1
                 in { model | elapsed = new,
                              countdownActive = new < model.maxSeconds }
            else model
  Play -> { model | countdownActive = not model.countdownActive }
  Stop -> { model | elapsed = 0, countdownActive = False }

subscriptions : Model -> Sub Msg
subscriptions model = Time.every second Tick

