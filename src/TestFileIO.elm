module TestFileIO exposing (..)

{-
   Sample elm calling program
-}

import Html exposing (Html, div, button, input, text, textarea)
import Html.Events exposing (onClick, on, onInput, targetValue)
import Html.Attributes exposing (..)
import Task exposing (..)
import String exposing (..)
import Result exposing (..)
import FileIO.Ports exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { text : String
    , name : Maybe String
    }


{-| initialise the model
-}
init : ( Model, Cmd Msg )
init =
    { text = ""
    , name = Nothing
    }
        ! [ Cmd.none ]



-- UPDATE


type Msg
    = NoOp
    | Text String
      -- get the text from the text area
    | RequestFileUpload
    | RequestFileDownload String
    | FileLoaded (Maybe Filespec)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Text s ->
            ( { model | text = s }, Cmd.none )

        RequestFileUpload ->
            ( model, requestLoadFile () )

        RequestFileDownload name ->
            let
                filespec =
                    Filespec model.text name
            in
                ( model, requestSaveFile filespec )

        FileLoaded maybef ->
            let
                _ =
                    log "elm filespec input" maybef
            in
                case maybef of
                    Just f ->
                        ( { model
                            | text = f.contents
                            , name = Just f.name
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )



-- SUBSCRIPTIONS


fileLoadedSub : Sub Msg
fileLoadedSub =
    fileLoaded FileLoaded


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileLoadedSub
        ]



-- EFFECTS
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ placeholder "abc"
            , Html.Attributes.value model.text
            , cols 70
            , rows 16
            , autocomplete False
            , spellcheck False
            , autofocus True
            , onInput Text
            ]
            []
        , div [] [ text ("file name: " ++ (model.name |> Maybe.withDefault "")) ]
        , input
            [ type_ "file"
            , id "fileinput"
            , accept ".abc"
              -- , property "media_type" (Json.string "text/vnd.abc")
            , on "change" (Json.succeed RequestFileUpload)
            ]
            []
        , button [ onClick (RequestFileDownload "abc/download.abc") ] [ text "save" ]
        ]
