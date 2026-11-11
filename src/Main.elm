module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Idle
        , view = view
        , update = update
        }


type Model
    = Idle
    | Moving XOffset YOffset
    | Scrolling ScrollProgress


type alias XOffset =
    Float


type alias YOffset =
    Float


type alias WidthPixels =
    Float


type alias HeightPixels =
    Float


type alias XPos =
    Float


type alias YPos =
    Float


type alias ScrolledDistance =
    Float


type alias ScrollProgress =
    Float


type Msg
    = PointerMoved WidthPixels HeightPixels XPos YPos
    | Scrolled HeightPixels ScrolledDistance


offset : Model -> ( XOffset, YOffset )
offset model =
    case model of
        Idle ->
            ( 0, 0 )

        Moving x y ->
            ( calibratedX x, calibratedY y )

        Scrolling prog ->
            ( negate <| prog * 3, negate prog )


calibratedX : Float -> Float
calibratedX =
    (*) 2


calibratedY : Float -> Float
calibratedY =
    (*) 1.1


wheelRotation : Model -> Html.Attribute msg
wheelRotation =
    offset
        >> Tuple.first
        >> (*) 360
        >> String.fromFloat
        >> (\o -> Attr.style "transform" ("rotate(" ++ o ++ "deg)"))


armRotation : Model -> Html.Attribute msg
armRotation =
    offset
        >> Tuple.second
        >> (*) 45
        >> negate
        >> String.fromFloat
        >> (\o -> Attr.style "transform" ("rotate(" ++ o ++ "deg)"))


xTransform : Model -> Html.Attribute msg
xTransform =
    offset
        >> Tuple.first
        >> (*) 32
        >> String.fromFloat
        >> (\o -> Attr.style "transform" ("translateX(" ++ o ++ "rem"))


decodePointerMoveEvent : Decoder Msg
decodePointerMoveEvent =
    Decode.map4 PointerMoved
        (Decode.at [ "view", "innerWidth" ] Decode.float)
        (Decode.at [ "view", "innerHeight" ] Decode.float)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


decodeScrollEvent : Decoder Msg
decodeScrollEvent =
    Decode.map2 Scrolled
        (Decode.at [ "target", "clientHeight" ] Decode.float)
        (Decode.at [ "target", "scrollTop" ] Decode.float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        PointerMoved width height x y ->
            Moving (calculateOffset width x) (calculateOffset height y)

        Scrolled height progress ->
            Scrolling <| (progress / height)


calculateOffset : Float -> Float -> Float
calculateOffset total pos =
    let
        half =
            total / 2
    in
    (pos
        - half
    )
        / half


view : Model -> Html Msg
view model =
    Html.main_
        [ Attr.class "overflow-y-scroll h-dvh"
        , Events.on "mousemove" decodePointerMoveEvent
        , Events.on "scroll" decodeScrollEvent
        ]
        [ Html.div
            [ Attr.class "w-wv h-dvh overflow-x-clip overflow-y-visible"
            , Attr.class "flex flex-col items-center justify-between"
            , Attr.class "bg-gif grain"
            ]
            [ Html.header [ Attr.class "text-center p-4 text-brand" ]
                [ Html.h1 [ Attr.class "text-3xl font-bold font-stretch-200% uppercase" ] [ Html.text "minigraving.pro" ]
                ]
            , Html.section [ Attr.class "mt-12 font-mono rounded-xl p-8 min-w-96 flex flex-col justify-center text-center items-center gap-2" ]
                [ Html.h2 [ Attr.class "text-5xl md:text-7xl text-white" ] [ Html.text "Graving, drenering og utleie" ]
                , Html.h2 [ Attr.class "text-3xl text-white" ] [ Html.text "– med og uten fører" ]
                , Html.a
                    [ Attr.class "flex gap-2 border-3 border-brand rounded-full py-2 px-8 mt-8 text-xl text-white"
                    , Attr.class "transition-colors hover:bg-brand"
                    , Attr.href "tel:90651267"
                    ]
                    [ Html.div [] [ Html.text "906" ]
                    , Html.div [] [ Html.text "51" ]
                    , Html.div [] [ Html.text "267" ]
                    ]
                ]
            , viewExcavator model
            ]
        , Html.div
            [ Attr.class "w-full h-dvh overflow-clip bg-dark grain"
            , Attr.class "flex flex-col justify-center items-center text-center"
            ]
            [ Html.section [ Attr.class "font-mono p-8 text-white text-3xl" ] [ Html.text "Her kommer det litt mer innhold etter hvert" ] ]
        ]


viewExcavator : Model -> Html msg
viewExcavator model =
    let
        selfAlignment =
            case model of
                Scrolling _ ->
                    Attr.class "self-end"

                _ ->
                    Attr.class "self-center"
    in
    Html.div
        [ Attr.class "flex flex-col"
        , Attr.class "[zoom:0.3] md:[zoom:0.6] z-20"
        , xTransform model
        , selfAlignment
        ]
        [ Html.div [ Attr.class "flex justify-center pr-6" ]
            [ Html.div [ Attr.class "bg-arm h-46 w-74 origin-[95%_80%]", armRotation model ] []
            , Html.div [ Attr.class "-ml-8 bg-house h-46 w-56" ] []
            ]
        , viewWheels model
        ]


viewWheels : Model -> Html msg
viewWheels model =
    let
        rotationAttr =
            model |> wheelRotation

        xTransformAttr =
            model |> xTransform

        wheel =
            Html.div
                [ Attr.class "size-16 m-2 rounded-full border-4 border-dashed"
                , Attr.class "flex justify-center items-center"
                , rotationAttr
                ]
                [ Html.div [ Attr.class "size-4 bg-dark" ] [] ]

        beltBorder =
            Html.div [ Attr.class "border-8 rounded-full size-20 border-dashed", rotationAttr ] []

        beltStretch =
            Html.div [ Attr.class "border-t-8 border-dashed w-[1000rem] ml-[-500rem] h-1", xTransformAttr ] []

        beltLeftEnd =
            Html.div [ Attr.class "absolute w-10 overflow-clip" ] [ beltBorder ]

        beltRightEnd =
            Html.div [ Attr.class "absolute right-0 w-10 overflow-clip rotate-180" ] [ beltBorder ]

        beltTop =
            Html.div [ Attr.class "absolute top-0 left-10 right-10 overflow-clip" ] [ beltStretch ]

        beltBottom =
            Html.div [ Attr.class "absolute bottom-0 left-10 right-10 overflow-clip rotate-180" ] [ beltStretch ]

        middle =
            Html.div [ Attr.class "absolute rounded-full h-8 left-8 right-8 bg-dark" ] []
    in
    Html.div [ Attr.class "relative self-end w-1/2 flex items-center justify-between" ]
        [ middle
        , beltLeftEnd
        , wheel
        , wheel
        , beltRightEnd
        , beltTop
        , beltBottom
        ]
