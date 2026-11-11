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
    | Moving XOffset YOffset ScrollProgress


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

        Moving x y prog ->
            ( calibratedX x prog, calibratedY y prog )


calibratedX : XOffset -> ScrollProgress -> Float
calibratedX x prog =
    x * 2 + (negate prog * 3)


calibratedY : YOffset -> ScrollProgress -> Float
calibratedY y prog =
    y * 1.1 + negate prog


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
    case ( model, msg ) of
        ( Idle, PointerMoved width height x y ) ->
            Moving (calculateOffset width x) (calculateOffset height y) 0

        ( Idle, Scrolled height progress ) ->
            Moving 0 0 (progress / height)

        ( Moving _ _ scroll, PointerMoved width height x y ) ->
            Moving (calculateOffset width x) (calculateOffset height y) scroll

        ( Moving x y _, Scrolled height progress ) ->
            Moving x y (progress / height)


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
        , Attr.class "overflow-y-scroll scroll-smooth [overflow-behavior:none]"
        , Events.on "mousemove" decodePointerMoveEvent
        , Events.on "scroll" decodeScrollEvent
        ]
        [ Html.div
            [ Attr.class "flex flex-col items-center justify-between"
            , Attr.class "h-dvh"
            , Attr.class "overflow-clip"
            , Attr.class "bg-gif grain"
            ]
            [ Html.header [ Attr.class "p-4 text-center text-brand" ]
                [ Html.h1 [ Attr.class "text-3xl font-bold font-stretch-200% uppercase" ] [ Html.text "minigraving.pro" ]
                , Html.h3 [ Attr.class "text-lg text-white font-mono" ] [ Html.text "– Små og store gravejobber i Grenland og omegn" ]
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
                    [ Attr.class "sticky top-2"
                    , Attr.class "flex gap-2"
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
            , Attr.class "min-h-dvh w-full"
            , Attr.class "overflow-clip"
            , Attr.class "text-center"
            , Attr.class "bg-dark bg-minigraver grain"
            ]
            [ viewMachineStats ]
        ]


type alias Stat =
    { label : String
    , value : String
    , unit : String
    }


machineStats : List Stat
machineStats =
    [ { label = "Vekt", value = "2590", unit = "kg" }
    , { label = "Motor", value = "20,9", unit = "hk" }
    , { label = "Gravedybde", value = "2,82", unit = "m" }
    , { label = "Rekkevidde", value = "4,74", unit = "m" }
    , { label = "Bredde", value = "1,5", unit = "m" }
    , { label = "Gravekraft skuff", value = "21", unit = "kN" }
    , { label = "Hjelpehydraulikk", value = "48", unit = "l/min" }
    , { label = "Kjørehastighet", value = "4,5", unit = "km/t" }
    ]


viewMachineStats : Html msg
viewMachineStats =
    Html.section
        [ Attr.class "flex flex-col items-center gap-8"
        , Attr.class "max-w-4xl p-8"
        , Attr.class "font-mono text-white"
        ]
        [ Html.header [ Attr.class "flex flex-col gap-2" ]
            [ Html.h2 [ Attr.class "text-4xl md:text-6xl font-bold font-stretch-200% uppercase text-brand" ] [ Html.text "Kubota U27-4" ]
            , Html.p [ Attr.class "text-lg" ] [ Html.text "Vi har en sterk og kompakt minigraver med null halesving, som kommer til der store maskiner ikke slipper inn. Vi leier ut med og uten fører, og graveren kan kjøres uten maskinførerbevis" ]
            ]
        , Html.dl [ Attr.class "grid grid-cols-2 md:grid-cols-4 gap-4 w-full" ]
            (List.map viewStat machineStats)
        , Html.p [ Attr.class "text-sm opacity-70" ] [ Html.text "Stage V-motor · Under 3,5 tonn med henger · ROPS/OPG-hytte" ]
        ]


viewStat : Stat -> Html msg
viewStat stat =
    Html.div
        [ Attr.class "flex flex-col-reverse gap-1"
        , Attr.class "p-4"
        , Attr.class "border-3 border-brand rounded-xl"
        ]
        [ Html.dt [ Attr.class "text-sm uppercase opacity-80" ] [ Html.text stat.label ]
        , Html.dd [ Attr.class "text-2xl md:text-3xl" ]
            [ Html.text stat.value
            , Html.span [ Attr.class "ml-1 text-base text-brand" ] [ Html.text stat.unit ]
            ]
        ]


viewExcavator : Model -> Html msg
viewExcavator model =
    Html.div
        [ Attr.class "flex flex-col self-end"
        , Attr.class "z-20"
        , Attr.class "[zoom:0.3] md:[zoom:0.6]"
        , xTransform model
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
