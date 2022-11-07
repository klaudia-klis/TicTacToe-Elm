module Main exposing (main)

import Browser
import Css exposing (hover)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import List.Extra exposing (setAt, getAt)

type alias Model =
  { turn : Marker
  , board : List (Maybe Marker)
  }

init : Model
init =
  { turn = X
  , board = List.repeat 9 Nothing 
  }

type Msg = SetCell Int | Restart
    
type Marker = X | O

type Winner
  = XWins (Int, Int, Int)
  | OWins (Int, Int, Int)
  | Draw
  | NoWinnerYet

showMarker : Marker -> String
showMarker marker =
  case marker of
    X -> "X"
    O -> "O"
        
isJust : Maybe a -> Bool
isJust m = case m of
  Just _ -> True
  Nothing -> False
        
winner : List (Maybe Marker) -> Winner
winner board = case board of
  -- top row
  Just X::Just X::Just X::_ -> XWins (0, 1, 2)
  Just O::Just O::Just O::_ -> OWins (0, 1, 2)
  -- middle row
  _::_::_::Just X::Just X::Just X::_ -> XWins (3, 4, 5)
  _::_::_::Just O::Just O::Just O::_ -> OWins (3, 4, 5)
  -- bottom row
  _::_::_::_::_::_::Just X::Just X::Just X::_ -> XWins (6, 7, 8)
  _::_::_::_::_::_::Just O::Just O::Just O::_ -> OWins (6, 7, 8)
  -- left column
  Just X::_::_::Just X::_::_::Just X::_ -> XWins (0, 3, 6)
  Just O::_::_::Just O::_::_::Just O::_ -> OWins (0, 3, 6)
  -- middle column
  _::Just X::_::_::Just X::_::_::Just X::_ -> XWins (1, 4, 7)
  _::Just O::_::_::Just O::_::_::Just O::_ -> OWins (1, 4, 7)
  -- right column
  _::_::Just X::_::_::Just X::_::_::Just X::_ -> XWins (2, 5, 8)
  _::_::Just O::_::_::Just O::_::_::Just O::_ -> OWins (2, 5, 8)
  -- top-left bottom-right diagonal
  Just X::_::_::_::Just X::_::_::_::Just X::_ -> XWins (0, 4, 8)
  Just O::_::_::_::Just O::_::_::_::Just O::_ -> OWins (0, 4, 8)
  -- bottom-left top-right diagonal
  _::_::Just X::_::Just X::_::Just X::_ -> XWins (2, 4, 6)
  _::_::Just O::_::Just O::_::Just O::_ -> OWins (2, 4, 6)

  xs -> if List.all isJust xs then Draw else NoWinnerYet

changeTurn : Marker -> Marker
changeTurn marker = case marker of
  X -> O
  O -> X

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetCell cell_ ->
      case winner model.board of
        XWins _ -> model
        OWins _ -> model
        Draw -> model
        NoWinnerYet ->
          if getAt cell_ model.board == Just Nothing
          then
            { model
                | board = setAt cell_ (Just model.turn) model.board
                , turn = changeTurn model.turn
            }
          else model
    Restart ->
      init

view : Model -> Html Msg
view model =
  case model.board of
    [a1, a2, a3, b1, b2, b3, c1, c2, c3] ->
      div [ style "text-align" "center" ]
        [ table [ style "border-collapse" "collapse"
                , style "margin" "auto"
                ]
            [ tr [] [ cell 0 (winner model.board) a1, cell 1 (winner model.board) a2, cell 2 (winner model.board) a3 ]
            , tr [] [ cell 3 (winner model.board) b1, cell 4 (winner model.board) b2, cell 5 (winner model.board) b3 ]
            , tr [] [ cell 6 (winner model.board) c1, cell 7 (winner model.board) c2, cell 8 (winner model.board) c3 ]
            ]
        , p [] 
            [ case winner model.board of
                XWins _ -> text "X wins"
                OWins _ -> text "O wins"
                Draw -> text "It's a draw"
                NoWinnerYet -> text ("It's " ++ showMarker model.turn ++ "'s turn.") ]
        , button [ style "height" "20px"
                 , style "font-family" "Times New Roman"
                 , style "font-size" "15px"
                 , style "cursor" "pointer"
                 , onClick Restart ] [ text "Restart" ]
        ]
    _ -> text "Invalid board"
        
cell : Int -> Winner -> Maybe Marker -> Html Msg
cell idx winner_ mMarker =
  let cellStyle =
        [ style "border-style" "solid"
        , style "border-color" "black"
        , style "border-width" "1px"
        , style "width" "100px"
        , style "height" "100px"
        , style "text-align" "center"
        , style "cursor" "pointer"
        , style "font-size" "60px"
        ]
      attrs =
        (onClick (SetCell idx) :: cellStyle)
      style_ =
        case winner_ of
          XWins (a, b, c) ->
            if List.member idx [a,b,c]
            then (style "background" "green" :: attrs)
            else attrs
          OWins (a, b, c) ->
            if List.member idx [a,b,c]
            then (style "background" "green" :: attrs)
            else attrs
          Draw ->
            attrs
          NoWinnerYet ->
            attrs
      content =
        case mMarker of
          Just marker -> [ text (showMarker marker) ]
          Nothing     -> [ text "" ]
   in td style_ content

main : Program () Model Msg
main = Browser.sandbox
  { init = init
  , view = view
  , update = update
  }