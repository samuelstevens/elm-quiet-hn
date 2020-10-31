port module Main exposing (..)

import Browser
import Dict
import Html
import Html.Attributes
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Story =
    { id : Int
    , url : String
    , title : String
    }


type alias Model =
    { stories : Dict.Dict Int Story, err : String, topIds : List Int }


type Msg
    = GotTopStories (Result Http.Error (List Int))
    | GotStory (Result Http.Error Story)


topStoriesDecoder : D.Decoder (List Int)
topStoriesDecoder =
    D.list D.int


storyDecoder : Int -> D.Decoder Story
storyDecoder id =
    D.succeed Story
        |> hardcoded id
        |> optional "url"
            D.string
            ("https://news.ycombinator.com/item?id=" ++ String.fromInt id)
        |> required "title" D.string


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( { stories = Dict.empty, err = "", topIds = [] }, Http.get { url = "https://hacker-news.firebaseio.com/v0/topstories.json", expect = Http.expectJson GotTopStories topStoriesDecoder } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTopStories result ->
            case result of
                Ok storyIds ->
                    let
                        top30 =
                            List.take 30 storyIds
                    in
                    ( { model | topIds = top30 }, Cmd.batch (List.map getStory top30) )

                Err err ->
                    case err of
                        Http.BadUrl url ->
                            ( { model | err = url }, Cmd.none )

                        Http.BadBody body ->
                            ( { model | err = body }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        GotStory result ->
            case result of
                Ok story ->
                    ( { model | stories = Dict.insert story.id story model.stories }, Cmd.none )

                Err err ->
                    case err of
                        Http.BadUrl url ->
                            ( { model | err = url }, Cmd.none )

                        Http.BadBody body ->
                            ( { model | err = body }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )



-- HTTP


getStory : Int -> Cmd Msg
getStory id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt id ++ ".json"
        , expect = Http.expectJson GotStory (storyDecoder id)
        }



-- PORTS


port setStorage : E.Value -> Cmd msg



-- SUBSCRIPTIONS


api_url : String
api_url =
    "https://hacker-news.firebaseio.com/v0/topstories.json"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Quiet HN - Elm"
        [ Html.h1 [] [ Html.text "Quiet Hacker News" ], Html.p [] [ Html.text model.err ], Html.ol [] (List.map viewStory (sortStories model.stories model.topIds)) ]


sortStories : Dict.Dict Int Story -> List Int -> List Story
sortStories stories topIds =
    List.filterMap (\key -> Dict.get key stories) topIds


viewStory : Story -> Html.Html Msg
viewStory story =
    Html.li [] [ Html.a [ Html.Attributes.href story.url ] [ Html.text story.title ] ]
