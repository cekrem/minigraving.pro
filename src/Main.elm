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
      -- TODO: Create unified holder of scroll state and pointer position
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
        [ Attr.class "h-dvh"
        , Attr.class "overflow-y-scroll scroll-smooth"
        , Events.on "mousemove" decodePointerMoveEvent
        , Events.on "scroll" decodeScrollEvent
        ]
        [ Html.div
            [ Attr.class "flex flex-col items-center justify-between"
            , Attr.class "h-dvh"
            , Attr.class "overflow-x-clip overflow-y-visible"
            , Attr.class "bg-gif grain"
            ]
            [ Html.header [ Attr.class "p-4 text-center text-brand" ]
                [ Html.h1 [ Attr.class "text-3xl font-bold font-stretch-200% uppercase" ] [ Html.text "minigraving.pro" ]
                , Html.h3 [ Attr.class "text-xl text-white font-mono" ] [ Html.text "– Små og store gravejobber i Grenland og omegn" ]
                ]
            , Html.section
                [ Attr.class "flex flex-col items-center justify-center gap-2"
                , Attr.class "min-w-96"
                , Attr.class "p-8 mt-12"
                , Attr.class "font-mono text-center"
                , Attr.class "rounded-xl"
                ]
                [ Html.h2 [ Attr.class "text-5xl md:text-7xl text-white" ] [ Html.text "Graving, drenering og maskinutleie" ]
                , Html.a
                    [ Attr.class "flex gap-2"
                    , Attr.class "px-8 py-2 mt-8"
                    , Attr.class "text-xl text-white"
                    , Attr.class "hover:bg-brand"
                    , Attr.class "border-3 border-brand rounded-full"
                    , Attr.class "transition-colors"
                    , Attr.href "tel:+4790651267"
                    ]
                    [ Html.div [] [ Html.text "+47" ]
                    , Html.div [] [ Html.text "906" ]
                    , Html.div [] [ Html.text "51" ]
                    , Html.div [] [ Html.text "267" ]
                    ]
                ]
            , viewExcavator model
            ]
        , Html.div
            [ Attr.class "flex flex-col items-center justify-center"
            , Attr.class "h-dvh w-full"
            , Attr.class "overflow-clip"
            , Attr.class "text-center"
            , Attr.class "bg-dark grain"
            ]
            [ Html.section [ Attr.class "p-8 font-mono text-3xl text-white" ] [ Html.text "Her kommer det litt mer innhold etter hvert" ] ]
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
        , Attr.class "z-20"
        , Attr.class "[zoom:0.3] md:[zoom:0.6]"
        , xTransform model
        , selfAlignment
        ]
        [ Html.div [ Attr.class "flex justify-center pr-6" ]
            [ Html.div [ Attr.class "h-46 w-74 bg-arm origin-[95%_80%]", armRotation model ] []
            , Html.div [ Attr.class "h-46 w-56 -ml-8 bg-house" ] []
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
                [ Attr.class "flex items-center justify-center"
                , Attr.class "size-16"
                , Attr.class "m-2"
                , Attr.class "border-4 border-dashed rounded-full"
                , rotationAttr
                ]
                [ Html.div [ Attr.class "size-4 bg-dark" ] [] ]

        beltBorder =
            Html.div [ Attr.class "size-20 border-8 border-dashed rounded-full", rotationAttr ] []

        beltStretch =
            Html.div [ Attr.class "h-1 w-[1000rem] ml-[-500rem] border-t-8 border-dashed", xTransformAttr ] []

        beltLeftEnd =
            Html.div [ Attr.class "absolute w-10 overflow-clip" ] [ beltBorder ]

        beltRightEnd =
            Html.div [ Attr.class "absolute right-0 w-10 overflow-clip rotate-180" ] [ beltBorder ]

        beltTop =
            Html.div [ Attr.class "absolute top-0 right-10 left-10 overflow-clip" ] [ beltStretch ]

        beltBottom =
            Html.div [ Attr.class "absolute right-10 bottom-0 left-10 overflow-clip rotate-180" ] [ beltStretch ]

        middle =
            Html.div [ Attr.class "absolute right-8 left-8 h-8 bg-dark rounded-full" ] []
    in
    Html.div
        [ Attr.class "flex"
        , Attr.class "relative"
        , Attr.class "w-1/2"
        , Attr.class "items-center justify-between self-end"
        ]
        [ middle
        , beltLeftEnd
        , wheel
        , wheel
        , beltRightEnd
        , beltTop
        , beltBottom
        ]
