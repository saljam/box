module Box where

import Debug

import Html
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import List
import Signal
import Signal ((<~), (~))
import String
import Svg
import Svg.Attributes
import Bitwise
import Text
import Json.Encode

segmentSizes : Float -> Float -> (Float, Float, Int)
segmentSizes length tabsize = 
  let
    segments = ((floor <| length / tabsize)//2) *2 + 1
  in
    (length / (toFloat segments), length / (toFloat segments), segments)

even : Int -> Bool
even i = i % 2 == 0

-- TODO replace flip with mirror transform
-- TODO simplify this mess...
-- TODO sort out kerf
side length tabsize kerf thickness tabstart flip =
  let
    (tablen'', gaplen'', ntab) = (segmentSizes length tabsize)
    last = ntab-1
    tablen' = tablen'' + kerf
    firsttab' = tablen' - kerf/2
    gaplen' = gaplen'' - kerf
    firstgap' = gaplen' + kerf/2
    (tablen, gaplen) = if flip then (gaplen', tablen') else (tablen', gaplen')
    (firsttab, firstgap) = if flip then (firstgap', firsttab') else (firsttab', firstgap')
    nthy n =
      let
        halfn = toFloat <| (n-1) // 2
      in
        (if tabstart then firsttab else firstgap)
        + tablen * halfn + gaplen * halfn
        + (if even (n-1) then 0 else
            (if tabstart then tablen else gaplen))
    pnt n = if (even n == tabstart) then [(0, nthy n), (0, nthy n + tablen)]
                                    else [(thickness, nthy n), (thickness, nthy n + gaplen)]
  in
    (if tabstart then [(0,0), (0, firsttab)]
                 else [(thickness, 0), (thickness, firstgap)])
    ++ List.concat (List.map pnt [1..last-1]) ++
    (if (even ntab == tabstart) then [(thickness, nthy last), (thickness, nthy last + firstgap)]
                                else [(0, nthy last), (0, nthy last + firsttab)])

translate x y = List.map (\(a,b) -> (x+a,y+b))

rotate = List.map (\(a,b) -> (b,a))

trim path prev next horizontal =
  let
    a = List.head path
    a' = List.head <| List.reverse prev
    z = List.head <| List.reverse path
    z' = List.head next
    body = List.take (List.length path - 2) <| List.tail path
  in
    if horizontal then (fst a', snd a) :: body ++ [(fst z', snd z)]
                  else (fst a, snd a') :: body ++ [(fst z, snd z')]

panel length width tabsize kerf thickness tabs =
  let
    left   = side length tabsize kerf thickness tabs.left False
    bottom = side width tabsize kerf thickness (not tabs.bottom) True
             |> rotate |> translate 0 (length-thickness)
    right  = side length tabsize kerf thickness (not tabs.right) True
             |> List.reverse |> translate (width-thickness) 0
    top    = side width tabsize kerf thickness tabs.top False
             |> rotate |> List.reverse
    left'   = trim left top bottom False
    bottom' = trim bottom left right True
    right'  = trim right bottom top False
    top'    = trim top right left True
  in
    left' ++ bottom' ++ right' ++ top'

pathString path =
  let
    pairs (x,y) = toString x ++ "," ++ toString y
  in
    String.join " " <| List.map pairs path

box width height length tabsize kerf thickness =
  let
    a = panel length width tabsize kerf thickness {top=True, bottom=True, left=True, right=True}
    b = panel length height tabsize kerf thickness {top=False, bottom=False, left=False, right=False}
    c = panel height width tabsize kerf thickness {top=False, bottom=False, left=True, right=True}
    awidth  = List.maximum <| List.map fst a
    aheight = List.maximum <| List.map snd a
    bwidth  = List.maximum <| List.map fst b
    cheight = List.maximum <| List.map snd c
    svgw = awidth + bwidth + 20
    svgh = aheight + cheight + 20
  in
    Svg.svg
      [ attribute "width" (toString svgw ++ "mm")
      , attribute "height" (toString svgh ++ "mm" )
      , attribute "viewBox" ("0 0 " ++ toString svgw ++ " " ++ toString svgh)
      ]
      [ Svg.g [Svg.Attributes.transform "translate(5,5)"]
        [ Svg.polyline [ Svg.Attributes.fill "none"
                       , Svg.Attributes.stroke "black"
                       , Svg.Attributes.strokeWidth "0.2mm"
                       , Svg.Attributes.points <| pathString <| a
                       ] []
        ]
      , Svg.g [Svg.Attributes.transform ("translate("++ toString (10+awidth) ++",5)")]
        [ Svg.polyline [ Svg.Attributes.fill "none"
                       , Svg.Attributes.stroke "black"
                       , Svg.Attributes.strokeWidth "0.2mm"
                       , Svg.Attributes.points <| pathString <| b
                       ] []
        ]
      , Svg.g [Svg.Attributes.transform ("translate(5,"++ toString (10+aheight) ++")")]
        [ Svg.polyline [ Svg.Attributes.fill "none"
                       , Svg.Attributes.stroke "black"
                       , Svg.Attributes.strokeWidth "0.2mm"
                       , Svg.Attributes.points <| pathString <| c
                       ] []
        ]
      ]

boxBlock wStr hStr lStr tabStr kerfStr thickStr =
  case (String.toFloat wStr, String.toFloat hStr, String.toFloat lStr, String.toFloat tabStr, String.toFloat kerfStr, String.toFloat thickStr) of
          (Ok w, Ok h, Ok l, Ok tab, Ok kerf, Ok thickness) ->
            box w h l tab kerf thickness
          otherwise ->
            Html.text "bad input :-/"

inputFor channel val =
  input [ type' "number", value val, on "input" targetValue (Signal.send channel) ] []

page w h l tab kerf thickness =
  div []
    [ h1 [] [ text "Box-Joint-O-Matic" ]
    , Html.form [] [ label [] [ text "outside dimentions "
                              , inputFor lengthc l
                              , text " × "
                              , inputFor widthc w
                              , text " × "
                              , inputFor heightc h
                              ]
                   , label [] [ text "tab width "
                              , inputFor tabc tab
                              ]
                   , label [] [ text "material thickness "
                              , inputFor thicknessc thickness
                              ]
                   , label [] [ text "kerf compensation "
                              , inputFor kerfc kerf
                              ]
                   ]
    , boxBlock w h l tab kerf thickness
    -- TODO add download link
    , footer [ class "mono" ] [ text "<3" ]
    ]

-- TODO replace the 6 arguments with a "model" record.

widthc = Signal.channel "100"
heightc = Signal.channel "50"
lengthc = Signal.channel "50"
tabc = Signal.channel "10"
kerfc = Signal.channel "0"
thicknessc = Signal.channel "3"

main : Signal.Signal Html.Html
main = page <~ (Signal.subscribe widthc)
             ~ (Signal.subscribe heightc)
             ~ (Signal.subscribe lengthc)
             ~ (Signal.subscribe tabc)
             ~ (Signal.subscribe kerfc)
             ~ (Signal.subscribe thicknessc)
