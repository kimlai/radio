port module Radio.Main exposing (..)

import Api
import Http
import Json.Decode exposing (field)
import Keyboard
import Model
import Navigation exposing (Location)
import Player
import PlayerEngine
import Radio.Model exposing (Model, Page(..), PlaylistId(..))
import Radio.Router
import Radio.Update as Update exposing (Msg(..))
import Radio.View as View
import Task
import Tracklist
import Update exposing (addCmd, andThen)


main : Program String Model Msg
main =
    Navigation.programWithFlags
        (\location -> NavigateTo (route location))
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


route : Location -> Page
route location =
    Radio.Router.urlToPage location.pathname


init : String -> Location -> ( Radio.Model.Model, Cmd Msg )
init flags location =
    let
        playlistId =
            Json.Decode.decodeString (field "playlistId" Json.Decode.int) flags
                |> Result.withDefault 0

        playlistUrl =
            "/public/json/playlists/" ++ toString playlistId ++ "/page_1.json"

        model =
            { tracks = Tracklist.empty
            , radio = Radio.Model.emptyPlaylist Radio playlistUrl
            , showRadioPlaylist = False
            , latestTracks = Radio.Model.emptyPlaylist LatestTracks "/public/json/tracks/page_1.json"
            , played = []
            , playing = False
            , currentPage = route location
            , lastKeyPressed = Nothing
            , player = Player.initialize [ Radio, LatestTracks ]
            , navigation = navigation
            }

        navigateToLocation =
            Update.update (NavigateTo (route location))

        initializeRadio =
            Http.send (FetchedMore Radio False) (Api.fetchPlaylist playlistUrl Api.decodeTrack)

        fetchLatestTracks =
            Update.update (FetchMore LatestTracks False)
    in
    model
        |> navigateToLocation
        |> andThen fetchLatestTracks
        |> addCmd initializeRadio



-- SUBSCRIPTIONS


subscriptions : Radio.Model.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PlayerEngine.trackProgress TrackProgress
        , PlayerEngine.trackEnd (\_ -> Next)
        , PlayerEngine.trackError TrackError
        , Keyboard.presses KeyPressed
        ]


navigation : List (Model.NavigationItem Radio.Model.Page PlaylistId)
navigation =
    [ Model.NavigationItem "Radio" "/" RadioPage (Just Radio)
    , Model.NavigationItem "All tracks" "/latest" LatestTracksPage (Just LatestTracks)
    ]
