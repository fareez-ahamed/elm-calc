module StarWars exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Character =
    { name : String
    , url : String
    }


type alias CharacterDetail =
    { name : String
    , weight : Int
    , height : Int
    }


type alias Model =
    { charlist : ApiResponse (List Character)
    , detail : CharacterDetailState
    }


type ApiResponse a
    = Loading
    | Success a
    | Failure


type CharacterDetailState
    = None
    | Active (ApiResponse CharacterDetail)


type Msg
    = GotCharacters (Result Http.Error (List Character))
    | GotCharacterDetail (Result Http.Error CharacterDetail)
    | LoadPokemonDetail String


init : a -> ( Model, Cmd Msg )
init _ =
    ( { charlist = Loading, detail = None }
    , loadPokemonList
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCharacters x ->
            case x of
                Ok c ->
                    ( { model | charlist = Success c }, Cmd.none )

                Err _ ->
                    ( { model | charlist = Failure }, Cmd.none )

        GotCharacterDetail x ->
            case x of
                Ok c ->
                    ( { model | detail = Success c |> Active }, Cmd.none )

                Err _ ->
                    ( { model | detail = Failure |> Active }, Cmd.none )

        LoadPokemonDetail url ->
            ( { model | detail = Loading |> Active }, loadPokemonDetail url )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model.charlist of
        Loading ->
            p [] [ text "Loading.." ]

        Success x ->
            div [] [topBar x, detail model.detail]

        Failure ->
            p [] [ text "Error in fetching the data" ]


pokeButton pokemon =
    button [ LoadPokemonDetail pokemon.url |> onClick ] [ text pokemon.name ]


topBar pokeList =
    let
        mapFn p =
            pokeButton p
    in
    div [] (List.map mapFn pokeList)


detail : CharacterDetailState -> Html Msg
detail charDetail =
    div []
        [ case charDetail of
            None ->
                p [] [ text "Click something to see the details" ]

            Active x ->
                case x of
                    Loading ->
                        p [] [ text "Loading" ] 

                    Success c ->
                        div []
                            [ p [] [ "Name : " ++ c.name |> text ]
                            , p [] [ "Height : " ++ (String.fromInt c.height) |> text ]
                            , p [] [ "Weight : " ++ (String.fromInt c.weight) |> text ]
                            ]
                    Failure ->
                        p [] [ text "Unable to fetch the data" ]
        ]


charDecoder : Decoder Character
charDecoder =
    map2 Character
        (field "name" string)
        (field "url" string)


charListDecoder : Decoder (List Character)
charListDecoder =
    field "results" (list charDecoder)


charDetailDecoder : Decoder CharacterDetail
charDetailDecoder =
    map3 CharacterDetail
        (field "name" string)
        (field "height" int)
        (field "weight" int)


loadPokemonList =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon"
        , expect = Http.expectJson GotCharacters charListDecoder
        }


loadPokemonDetail url =
    Http.get
        { url = url
        , expect = Http.expectJson GotCharacterDetail charDetailDecoder
        }
