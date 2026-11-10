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


type Msg
    = PointerMoved WidthPixels HeightPixels XPos YPos


offset : Model -> ( XOffset, YOffset )
offset model =
    case model of
        Idle ->
            ( 0, 0 )

        Moving x y ->
            ( x, y )


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


decodeMouseEvent : Decoder Msg
decodeMouseEvent =
    Decode.map4 PointerMoved
        (Decode.at [ "view", "innerWidth" ] Decode.float)
        (Decode.at [ "view", "innerHeight" ] Decode.float)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        PointerMoved width height x y ->
            Moving (calculateOffsetPercentage width x) (calculateOffsetPercentage height y)


calculateOffsetPercentage : Float -> Float -> Float
calculateOffsetPercentage total pos =
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
        [ Attr.class "h-screen v-screen overflow-clip"
        , Attr.class "flex flex-col items-center justify-between"
        , Attr.class "bg-neutral-400"
        , Events.on "pointermove" decodeMouseEvent
        ]
        [ Html.header [ Attr.class "text-center p-4 text-dark" ]
            [ Html.h1 [ Attr.class "text-2xl font-bold font-stretch-200% uppercase" ] [ Html.text "minigraving.pro" ]
            ]
        , Html.p [] [ Html.text "{ Her kommer det noe innhold }" ]
        , viewExcavator model
        ]


viewExcavator : Model -> Html msg
viewExcavator model =
    Html.div [ Attr.class "flex flex-col", xTransform model ]
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
                [ Html.div [ Attr.class "size-4 bg-brand" ] [] ]

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
