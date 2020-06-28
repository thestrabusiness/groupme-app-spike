module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , field
        , int
        , list
        , nullable
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Pipeline
    exposing
        ( custom
        , hardcoded
        , optional
        , required
        , requiredAt
        )



---- MODEL ----


type Model
    = ViewingGroups (List Group)
    | ViewingMessages String (List Message)


init : ( Model, Cmd Msg )
init =
    ( ViewingGroups [], getGroups )


type alias GroupMeResponse a =
    { response : a
    }


type alias Group =
    { id : String, name : String }


type alias Message =
    { id : String
    , createdAt : Int
    , text : Maybe String
    , authorName : String
    , avatarUrl : String
    , attachments : List Attachment
    , favoritesCount : Int
    }


type Attachment
    = Image { url : String }
    | Location { lat : String, lng : String, name : String }
    | Split { token : String }
    | Emoji { placeholder : String, charMap : List (List Int) }



---- UPDATE ----


type Msg
    = NoOp
    | GotGroups (Result Http.Error (GroupMeResponse (List Group)))
    | GotMessages String (Result Http.Error (GroupMeResponse (List Message)))
    | UserSelectedGroup String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotGroups result ->
            case result of
                Ok { response } ->
                    ( ViewingGroups response, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )

        GotMessages groupId result ->
            case result of
                Ok { response } ->
                    ( ViewingMessages groupId response, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )

        UserSelectedGroup groupId ->
            ( model, getMessages groupId )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        ViewingGroups groups ->
            div [] <| List.map viewGroup groups

        ViewingMessages _ messages ->
            div [] <| List.map viewMessage messages


viewGroup : Group -> Html Msg
viewGroup group =
    div [] [ h2 [ onClick <| UserSelectedGroup group.id ] [ text group.name ] ]


viewMessage : Message -> Html Msg
viewMessage message =
    div [] [ text <| Maybe.withDefault "" message.text ]



---- API ----


baseApiUrl : String
baseApiUrl =
    "https://api.groupme.com/v3"


groupsUrl : String
groupsUrl =
    baseApiUrl ++ "/groups"


messagesUrl : String -> String
messagesUrl groupId =
    groupsUrl ++ "/" ++ groupId ++ "/messages"


apiTokenParam : String
apiTokenParam =
    Debug.todo "Add your GroupMe API token here: ?token="


urlWithToken : String -> String
urlWithToken url =
    url ++ apiTokenParam


getGroups : Cmd Msg
getGroups =
    Http.get
        { url = urlWithToken groupsUrl
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


getMessages : String -> Cmd Msg
getMessages groupId =
    Http.get
        { url = urlWithToken <| messagesUrl groupId
        , expect = Http.expectJson (GotMessages groupId) messageListDecoder
        }


messageListDecoder : Decoder (GroupMeResponse (List Message))
messageListDecoder =
    succeed GroupMeResponse
        |> requiredAt [ "response", "messages" ] (list messageDecoder)


messageDecoder : Decoder Message
messageDecoder =
    succeed Message
        |> required "id" string
        |> required "created_at" int
        |> required "text" (nullable string)
        |> required "name" string
        |> required "avatar_url" string
        |> required "attachments" (list attachmentDecoder)
        |> hardcoded 0


attachmentDecoder : Decoder Attachment
attachmentDecoder =
    oneOf [ imageDecoder, locationDecoder, splitDecoder, emojiDecoder ]


imageDecoder : Decoder Attachment
imageDecoder =
    succeed imageFromResponse
        |> required "url" string


locationDecoder : Decoder Attachment
locationDecoder =
    succeed locationFromResponse
        |> required "lat" string
        |> required "lng" string
        |> required "name" string


splitDecoder : Decoder Attachment
splitDecoder =
    succeed splitFromResponse
        |> required "token" string


emojiDecoder : Decoder Attachment
emojiDecoder =
    succeed emojiFromResponse
        |> required "placeholder" string
        |> hardcoded []


imageFromResponse : String -> Attachment
imageFromResponse url =
    Image { url = url }


locationFromResponse : String -> String -> String -> Attachment
locationFromResponse lat lng name =
    Location { lat = lat, lng = lng, name = name }


splitFromResponse : String -> Attachment
splitFromResponse token =
    Split { token = token }


emojiFromResponse : String -> List (List Int) -> Attachment
emojiFromResponse placeholder charMap =
    Emoji { placeholder = placeholder, charMap = charMap }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
