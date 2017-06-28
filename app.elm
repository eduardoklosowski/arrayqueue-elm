module Main exposing (..)

import Array
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import List
import String exposing (toInt)


-- Queue


type alias Queue =
    { array : Array.Array (Maybe Int)
    , head : Int
    , tail : Int
    , size : Int
    }


initialize : Int -> Queue
initialize n =
    { array = Array.repeat n Nothing
    , head = 0
    , tail = 0
    , size = 0
    }


enqueue : Int -> Queue -> Queue
enqueue n queue =
    let
        q =
            if queue.size > 0 && queue.head == queue.tail then
                resize queue
            else
                queue

        plusTail =
            q.tail + 1

        newTail =
            if plusTail == (Array.length q.array) then
                0
            else
                plusTail
    in
        { q
            | array = Array.set q.tail (Just n) q.array
            , tail = newTail
            , size = q.size + 1
        }


dequeue : Queue -> ( Maybe Int, Queue )
dequeue queue =
    let
        e =
            front queue
    in
        case e of
            Just n ->
                ( Just n
                , { queue
                    | array = Array.set queue.head Nothing queue.array
                    , head = queue.head + 1
                    , size = queue.size - 1
                  }
                )

            Nothing ->
                ( Nothing, queue )


front : Queue -> Maybe Int
front queue =
    let
        e =
            Array.get queue.head queue.array
    in
        case e of
            Just n ->
                n

            Nothing ->
                Nothing


resize : Queue -> Queue
resize queue =
    let
        size =
            Array.length queue.array

        newArray =
            Array.append
                (Array.slice queue.head size queue.array)
                (Array.append (Array.slice 0 queue.head queue.array) (Array.repeat size Nothing))
    in
        { array = newArray
        , head = 0
        , tail = size
        , size = size
        }



-- Program


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { initialize : Maybe Int
    , enqueue : Maybe Int
    , dequeue : Maybe Int
    , front : Maybe Int
    , queue : Maybe Queue
    }


init : Model
init =
    { initialize = Just 10
    , enqueue = Nothing
    , dequeue = Nothing
    , front = Nothing
    , queue = Nothing
    }


type Msg
    = UpdateInitialize String
    | UpdateEnqueue String
    | UpdateDequeue String
    | UpdateFront String
    | Initialize
    | Enqueue
    | Dequeue
    | Front


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInitialize s ->
            case toInt s of
                Ok n ->
                    { model | initialize = Just n }

                Err _ ->
                    { model | initialize = Nothing }

        UpdateEnqueue s ->
            case toInt s of
                Ok n ->
                    { model | enqueue = Just n }

                Err _ ->
                    { model | enqueue = Nothing }

        UpdateDequeue s ->
            case toInt s of
                Ok n ->
                    { model | dequeue = Just n }

                Err _ ->
                    { model | dequeue = Nothing }

        UpdateFront s ->
            case toInt s of
                Ok n ->
                    { model | front = Just n }

                Err _ ->
                    { model | front = Nothing }

        Initialize ->
            case model.initialize of
                Just n ->
                    { model | queue = Just (initialize n) }

                Nothing ->
                    model

        Enqueue ->
            case model.queue of
                Just queue ->
                    case model.enqueue of
                        Just n ->
                            { model | queue = Just (enqueue n queue) }

                        Nothing ->
                            model

                Nothing ->
                    model

        Dequeue ->
            case model.queue of
                Just queue ->
                    let
                        ( e, newQueue ) =
                            dequeue queue
                    in
                        case e of
                            Just n ->
                                { model | queue = Just newQueue, dequeue = Just n }

                            Nothing ->
                                { model | dequeue = Nothing }

                Nothing ->
                    model

        Front ->
            case model.queue of
                Just queue ->
                    case front queue of
                        Just n ->
                            { model | front = Just n }

                        Nothing ->
                            { model | front = Nothing }

                Nothing ->
                    model



-- View


view model =
    Html.div []
        [ displayOperationsList model
        , displayQueue model.queue
        ]


displayOperationsList model =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Operations" ]
        , Html.div [ class "content" ]
            [ displayOperation "Queue size:" model.initialize "Initialize" UpdateInitialize Initialize
            , displayOperation "Value:" model.enqueue "Enqueue" UpdateEnqueue Enqueue
            , displayOperation "Value:" model.dequeue "Dequeue" UpdateDequeue Dequeue
            , displayOperation "Value:" model.front "Front" UpdateFront Front
            ]
        ]


displayOperation label i textButton updateAction clickAction =
    let
        v =
            case i of
                Just n ->
                    toString n

                Nothing ->
                    ""

        enable =
            case clickAction of
                Initialize ->
                    True

                Enqueue ->
                    True

                _ ->
                    False
    in
        Html.div [ class "operation" ]
            [ Html.div [ class "operation-label" ] [ Html.text label ]
            , Html.form [ onSubmit clickAction ] [ Html.input [ type_ "number", value v, onInput updateAction, disabled (not enable) ] [] ]
            , Html.button [ onClick clickAction ] [ Html.text textButton ]
            ]


displayQueue queue =
    let
        content =
            case queue of
                Just q ->
                    List.map (\( i, v ) -> displayNode q.head q.tail i v) (Array.toIndexedList q.array)

                Nothing ->
                    []
    in
        Html.fieldset []
            [ Html.legend [] [ Html.text "Queue state" ]
            , Html.div [ class "content" ] content
            ]


displayNode head tail i v =
    let
        vDisplay =
            case v of
                Just n ->
                    toString n

                Nothing ->
                    "_"

        hDisplay =
            if i == head then
                Html.span [ class "pointer head" ] [ Html.text "H" ]
            else
                Html.text ""

        tDisplay =
            if i == tail then
                Html.span [ class "pointer tail" ] [ Html.text "T" ]
            else
                Html.text ""
    in
        Html.div [ class "queue-element" ]
            [ Html.div [ class "queue-value" ] [ Html.text vDisplay ]
            , Html.div [ class "queue-pointers" ] [ hDisplay, tDisplay ]
            ]
