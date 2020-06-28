module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)



---- MODEL ----


type alias Model =
    { groups : List Group }


init : ( Model, Cmd Msg )
init =
    ( { groups = [] }, getGroups )


type alias GroupMeResponse a =
    { response : a
    }


type alias Group =
    { id : String, name : String }



---- UPDATE ----


type Msg
    = NoOp
    | GotGroups (Result Http.Error (GroupMeResponse (List Group)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotGroups result ->
            case result of
                Ok { response } ->
                    ( { model | groups = response }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] <| List.map viewGroup model.groups


viewGroup : Group -> Html Msg
viewGroup group =
    div [] [ h2 [] [ text group.name ] ]



---- API ----


baseApiUrl : String
baseApiUrl =
    "https://api.groupme.com/v3"


groupsUrl : String
groupsUrl =
    baseApiUrl ++ "/groups" ++ apiTokenParam


apiTokenParam : String
apiTokenParam =
    Debug.todo "Add your GroupMe API token here: ?token="


getGroups : Cmd Msg
getGroups =
    Http.get
        { url = groupsUrl
        , expect = Http.expectJson GotGroups groupListDecoder
        }


groupDecoder : Decoder Group
groupDecoder =
    succeed Group
        |> required "id" string
        |> required "name" string


groupListDecoder : Decoder (GroupMeResponse (List Group))
groupListDecoder =
    succeed GroupMeResponse
        |> required "response" (list groupDecoder)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
