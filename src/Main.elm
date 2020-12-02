port module Main exposing (..)

import Browser
import Dict
import Html
import Html.Attributes
import Http
import Json.Decode as D
import Json.Encode as E
import Task
import Time



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }



-- TYPES


type alias Url =
    String


type alias Id =
    Int


type alias StoryInfo =
    { id : Int
    , url : Url
    , title : String
    }


type Story
    = Loaded StoryInfo
    | Id { id : Id }


makeLoaded : Id -> Url -> String -> Story
makeLoaded id url title =
    Loaded { id = id, url = url, title = title }


makeId : Int -> Story
makeId id =
    Id { id = id }


isHNSpecific : StoryInfo -> Bool
isHNSpecific story =
    String.startsWith "Ask HN:" story.title 


type Time
    = Never
    | At Time.Posix


type alias Model =
    { stories : List Story, err : Maybe String, lastUpdated : Time }



-- MSG


type Msg
    = GotTopStories (Result Http.Error (List Int))
    | GotStory (Result Http.Error Story)
    | LoadedFromLocalStorage Model Time.Posix
    | UpdatedCache Time.Posix



-- DECODE


topStoriesDecoder : D.Decoder (List Int)
topStoriesDecoder =
    D.list D.int


loadedStoryDecoder : D.Decoder Story
loadedStoryDecoder =
    D.map3 makeLoaded
        (D.field "id" D.string
            |> D.andThen
                (\val ->
                    case String.toInt val of
                        Just i ->
                            D.succeed i

                        Nothing ->
                            D.fail "item id is not an integer."
                )
        )
        (D.field "url" D.string)
        (D.field "title" D.string)


idStoryDecoder : D.Decoder Story
idStoryDecoder =
    D.map makeId (D.field "id" D.int)


storyHelp : Maybe String -> D.Decoder Story
storyHelp s =
    case s of
        Just title ->
            loadedStoryDecoder

        Nothing ->
            idStoryDecoder


diskStoryDecoder : D.Decoder Story
diskStoryDecoder =
    D.nullable (D.field "title" D.string) |> D.andThen storyHelp


makeUrl : Id -> Maybe String -> Url
makeUrl id maybe =
    case maybe of
        Just url ->
            url

        Nothing ->
            "https://news.ycombinator.com/item?id=" ++ String.fromInt id


apiStoryHelp : Id -> D.Decoder Story
apiStoryHelp id =
    D.map3 makeLoaded
        (D.succeed id)
        (D.map (makeUrl id) (D.maybe (D.field "url" D.string)))
        (D.field "title" D.string)


apiStoryDecoder : D.Decoder Story
apiStoryDecoder =
    D.field "id" D.int |> D.andThen apiStoryHelp


lastUpdatedDecoder : D.Decoder Time
lastUpdatedDecoder =
    D.maybe
        (D.field "lastUpdated" D.int)
        |> D.andThen
            (\val ->
                case val of
                    Just t ->
                        D.succeed (At (Time.millisToPosix t))

                    Nothing ->
                        D.succeed Never
            )


modelDecoder : D.Decoder Model
modelDecoder =
    D.map3 Model
        (D.field "stories" (D.list diskStoryDecoder))
        (D.succeed (Just ""))
        lastUpdatedDecoder



-- ENCODE


encodeStory : Story -> E.Value
encodeStory story =
    case story of
        Loaded { title, url, id } ->
            E.object
                [ ( "id", E.string (String.fromInt id) )
                , ( "title", E.string title )
                , ( "url", E.string url )
                ]

        Id { id } ->
            E.object [ ( "id", E.string (String.fromInt id) ) ]


encodeModel : Model -> E.Value
encodeModel model =
    case model.lastUpdated of
        At t ->
            E.object
                [ ( "stories", E.list encodeStory model.stories )
                , ( "lastUpdated", E.int (Time.posixToMillis t) )
                ]

        Never ->
            E.object [ ( "stories", E.list encodeStory model.stories ) ]


{-| If a is an an hour or more later than b
-}
hourLater : Time.Posix -> Time.Posix -> Bool
hourLater a b =
    let
        millisA =
            Time.posixToMillis a

        millisB =
            Time.posixToMillis b + 1000 * 60 * 60
    in
    millisA > millisB



-- INIT


init : E.Value -> ( Model, Cmd Msg )
init args =
    case D.decodeValue modelDecoder args of
        Ok model ->
            ( model, Task.perform (LoadedFromLocalStorage model) Time.now )

        Err err ->
            case err of
                D.Field field msg ->
                    ( { stories = [], err = Nothing, lastUpdated = Never }, getTopStories )

                D.Failure fail _ ->
                    ( { stories = [], err = Nothing, lastUpdated = Never }, getTopStories )

                _ ->
                    ( { stories = [], err = Just "Unknown error", lastUpdated = Never }, getTopStories )



-- UPDATE


updateStoryList : Story -> List Story -> List Story
updateStoryList newStory stories =
    case newStory of
        Loaded story ->
            List.map
                (\s ->
                    case s of
                        Loaded loaded ->
                            Loaded loaded

                        Id { id } ->
                            if id == story.id then
                                Loaded { id = id, title = story.title, url = story.url }

                            else
                                Id { id = id }
                )
                stories

        Id _ ->
            stories


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTopStories result ->
            case result of
                Ok storyIds ->
                    let
                        top50 =
                            List.take 50 storyIds
                    in
                    ( { model | stories = List.map makeId top50 }, Cmd.batch (List.map getStory top50) )

                Err err ->
                    case err of
                        Http.BadUrl url ->
                            ( { model | err = Just url }, Cmd.none )

                        Http.BadBody body ->
                            ( { model | err = Just body }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        GotStory result ->
            case result of
                Ok story ->
                    let
                        newModel =
                            { model | stories = updateStoryList story model.stories }
                    in
                    ( newModel, Task.perform UpdatedCache Time.now )

                Err err ->
                    case err of
                        Http.BadUrl url ->
                            ( { model | err = Just url }, Cmd.none )

                        Http.BadBody body ->
                            ( { model | err = Just body }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        LoadedFromLocalStorage diskModel now ->
            case diskModel.lastUpdated of
                At updatedTime ->
                    if hourLater now updatedTime then
                        -- stories were updated more than an hour ago -> update them
                        ( model, getTopStories )

                    else
                        -- stories were updated less than an hour ago, just convert list to dict
                        ( diskModel, Cmd.none )

                Never ->
                    ( model, getTopStories )

        UpdatedCache now ->
            ( { model | lastUpdated = At now }, setStorage (encodeModel { model | lastUpdated = At now }) )



-- HTTP


getTopStories : Cmd Msg
getTopStories =
    Http.get { url = "https://hacker-news.firebaseio.com/v0/topstories.json", expect = Http.expectJson GotTopStories topStoriesDecoder }


getStory : Int -> Cmd Msg
getStory id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt id ++ ".json"
        , expect = Http.expectJson GotStory apiStoryDecoder
        }



-- PORTS


port setStorage : E.Value -> Cmd msg



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Quiet HN - Elm"
        [ Html.main_ []
            [ Html.h1 [] [ Html.text "Quiet Hacker News" ]
            , viewErr model.err
            , Html.ol [] ((List.filterMap viewStory model.stories) |> List.take 30)
            , Html.p []
                [ Html.a [ Html.Attributes.href "/projects/quiet-hn" ] [ Html.text "[About]" ]
                , Html.text " "
                , Html.a [ Html.Attributes.href "https://github.com/samuelstevens/elm-quiet-hn" ] [ Html.text "[Source]" ]
                ]
            ]
        ]


viewErr : Maybe String -> Html.Html Msg
viewErr err =
    case err of
        Just msg ->
            Html.p [] [ Html.text msg ]

        Nothing ->
            Html.text ""


viewStory : Story -> Maybe (Html.Html Msg)
viewStory story =
    case story of
        Loaded loaded ->
            if isHNSpecific loaded then
                Nothing

            else
                Just
                    (Html.li [] [ Html.a [ Html.Attributes.href loaded.url ] [ Html.text loaded.title ] ])

        Id _ ->
            Nothing
