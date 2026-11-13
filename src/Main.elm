module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy exposing (lazy)
import HtmlHelpers
import Json.Decode as Decode exposing (Decoder)



-- main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- model


type alias Model =
    { mouseX : MouseX
    , mouseY : MouseY
    , scrollProgress : ScrollProgress
    }


init : Model
init =
    Model 0 0 0


offset : Model -> ( XOffset, YOffset )
offset { mouseX, mouseY, scrollProgress } =
    ( calibratedXOffset mouseX scrollProgress, calibratedYOffset mouseY scrollProgress )


calibratedXOffset : MouseX -> ScrollProgress -> XOffset
calibratedXOffset x prog =
    x * 2 + (negate prog * 2)


calibratedYOffset : MouseY -> ScrollProgress -> YOffset
calibratedYOffset y prog =
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



-- update


type Msg
    = PointerMoved WidthPixels HeightPixels XPos YPos
    | Scrolled HeightPixels ScrolledDistance


update : Msg -> Model -> Model
update msg model =
    case msg of
        PointerMoved width height x y ->
            { model
                | mouseX = calculateOffset width x
                , mouseY = calculateOffset height y
            }

        Scrolled height progress ->
            { model | scrollProgress = progress / height }


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



-- view


view : Model -> Html Msg
view =
    lazy
        (\model ->
            let
                viewModelPage =
                    viewPage model
            in
            Html.main_
                [ Attr.class "h-dvh"
                , Attr.class "overflow-y-scroll scroll-smooth [overflow-behavior:none]"
                , Attr.class "select-none"
                , Events.on "mousemove" decodePointerMoveEvent
                , Events.on "scroll" decodeScrollEvent
                ]
                [ viewHeader model
                , -- first page
                  viewModelPage
                    0
                    [ Attr.class "bg-gif grain overflow-x-clip" ]
                    [ Html.section
                        [ Attr.class "flex flex-col items-center justify-center gap-2"
                        , Attr.class "min-w-96"
                        , Attr.class "p-8"
                        , Attr.class "font-mono text-center"
                        , Attr.class "rounded-xl"
                        , Attr.class "text-white"
                        ]
                        [ Html.h2 [ Attr.class "text-5xl md:text-7xl" ]
                            [ Html.text "Graving, drenering og maskinutleie"
                            ]
                        , Html.h3 [ Attr.class "text-lg font-bold" ]
                            [ Html.text "Små og store gravejobber i Grenland og omegn"
                            ]
                        , viewCallButton
                        ]
                    , viewExcavator model
                    ]

                -- second page
                , viewModelPage
                    1
                    [ Attr.class "bg-minigraver grain overflow-clip"
                    ]
                    [ Html.section
                        [ Attr.class "flex flex-col items-center gap-8"
                        , Attr.class "max-w-4xl p-8"
                        , Attr.class "font-mono text-white"
                        ]
                        [ viewMachineStats
                        ]
                    ]

                -- third page
                , viewModelPage 2
                    [ Attr.class "bg-dark grain"
                    ]
                    [ Html.section
                        [ Attr.class "flex flex-col items-stretch gap-8"
                        , Attr.class "max-w-4xl p-8"
                        , Attr.class "font-mono text-white"
                        ]
                        [ Html.div [ Attr.class "flex flex-col gap-2" ]
                            [ Html.h2 [ logoTypography ] [ Html.text "Priser, gravearbeid" ]
                            , Html.p [ Attr.class "text-lg font-bold" ] [ Html.text "Etter avtale" ]
                            ]
                        , Html.div [ Attr.class "flex flex-col gap-2" ]
                            [ Html.h2 [ logoTypography ] [ Html.text "Priser, utleie" ]
                            , Html.p [ Attr.class "text-lg font-bold" ]
                                [ viewPrice "Per dag" 1500
                                , viewPrice "Per helg" 3000
                                , viewPrice "Per uke" 8000
                                ]
                            ]
                        ]
                    ]

                -- fixed footer, not shown on first page
                , viewFooter model
                ]
        )


viewHeader : Model -> Html msg
viewHeader { scrollProgress } =
    let
        evenPage =
            (scrollProgress
                |> (+) 0.05
                |> floor
                |> modBy 2
            )
                == 0

        colorAttr =
            if evenPage then
                Attr.class "text-brand"

            else
                Attr.class "text-dark"
    in
    Html.header
        [ Attr.class "fixed p-4 z-15 w-full text-center transition-colors"
        , colorAttr
        ]
        [ Html.h1 [ logoTypography ] [ Html.text "minigraving.pro" ]
        ]


viewPage : Model -> Int -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewPage model index attrs =
    Html.div
        (Attr.class "flex flex-col items-center justify-center"
            :: Attr.class "h-dvh w-full"
            :: Attr.class "relative z-10"
            :: (model |> pageOpacity index)
            :: attrs
        )


viewFooter : Model -> Html msg
viewFooter { scrollProgress } =
    let
        visibilityAttr =
            if scrollProgress >= 0.8 then
                Attr.style "opacity" (String.fromFloat scrollProgress)

            else
                Attr.class "opacity-0"
    in
    Html.footer
        [ Attr.class "z-100 fixed bottom-8 w-full flex justify-center transition-opacity"
        , visibilityAttr
        ]
        [ viewCallButton ]


pageOpacity : Int -> Model -> Html.Attribute msg
pageOpacity index { scrollProgress } =
    let
        d =
            clamp 0 1 (abs (toFloat index - scrollProgress))
    in
    (1 + cos (pi * d))
        / 2
        |> (+) 0.2
        |> String.fromFloat
        |> Attr.style "opacity"


logoTypography : Html.Attribute msg
logoTypography =
    Attr.class "text-3xl font-sans font-bold font-stretch-200% uppercase"


viewCallButton : Html msg
viewCallButton =
    Html.a
        [ Attr.class "self-center"
        , Attr.class "flex gap-2"
        , Attr.class "px-8 py-2 mt-8"
        , Attr.class "text-xl text-white"
        , Attr.class "hover:bg-brand"
        , Attr.class "border-3 border-brand rounded-full"
        , Attr.class "transition-colors"
        , Attr.href "tel:+4790651267"
        , Attr.title "Ring for en uforpliktende samtale"
        ]
        [ Html.div [] [ Html.text "+47" ]
        , Html.div [] [ Html.text "906" ]
        , Html.div [] [ Html.text "51" ]
        , Html.div [] [ Html.text "267" ]
        ]



-- animated excavator


viewExcavator : Model -> Html msg
viewExcavator model =
    Html.div
        [ Attr.class "absolute bottom-0 flex flex-col self-end"
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



-- stats


type alias Stat =
    { label : String
    , value : String
    , unit : String
    }


machineStats : List Stat
machineStats =
    [ { label = "Vekt", value = "2590", unit = "kg" }
    , { label = "Motor", value = "19,6", unit = "hk" }
    , { label = "Gravedybde", value = "2,82", unit = "m" }
    , { label = "Rekkevidde", value = "4,74", unit = "m" }
    , { label = "Bredde", value = "1,5", unit = "m" }
    , { label = "Gravekraft skuff", value = "21", unit = "kN" }

    --    , { label = "Hjelpehydraulikk", value = "48", unit = "l/min" }
    --   , { label = "Kjørehastighet", value = "4,5", unit = "km/t" }
    ]


viewMachineStats : Html msg
viewMachineStats =
    [ Html.header [ Attr.class "flex flex-col gap-2" ]
        [ Html.h2 [ logoTypography ] [ Html.text "Kubota U27-4" ]
        , Html.p [ Attr.class "text-lg font-bold" ] [ Html.text "Vi har en sterk og kompakt minigraver med null halesving, som kommer til der store maskiner ikke slipper inn. Vi leier ut med og uten fører, og graveren kan kjøres uten maskinførerbevis." ]
        ]
    , Html.dl [ Attr.class "grid grid-cols-2 md:grid-cols-3 gap-4 w-full" ]
        (List.map viewStat machineStats)
    ]
        |> HtmlHelpers.wrapToSingleNode


viewStat : Stat -> Html msg
viewStat stat =
    Html.div
        [ Attr.class "flex flex-col-reverse gap-1"
        , Attr.class "p-4 text-center"
        , Attr.class "bg-dark rounded-xl"
        ]
        [ Html.dt [ Attr.class "text-xs", logoTypography ] [ Html.text stat.label ]
        , Html.dd [ Attr.class "text-2xl md:text-3xl" ]
            [ Html.text stat.value
            , Html.span [ Attr.class "ml-1 text-base text-brand" ] [ Html.text stat.unit ]
            ]
        ]



-- price / info


viewPrice : String -> Int -> Html msg
viewPrice label price =
    Html.div [ Attr.class "flex justify-between" ]
        [ Html.div [] [ Html.text label ]
        , Html.div [ Attr.class "text-brand" ]
            [ Html.text <| String.fromInt price
            , Html.text ",- "
            , Html.text "eks. mva"
            ]
        ]



-- decoders


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



-- type alias "decorators"


type alias MouseX =
    Float


type alias MouseY =
    Float


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
