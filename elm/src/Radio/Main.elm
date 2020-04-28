port module Radio.Main exposing (..)

import Api
import Http
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
import Time exposing (Time)
import Tracklist
import Update exposing (addCmd, andThen)


main : Program Never Model Msg
main =
    Navigation.program
        (\location -> NavigateTo (route location))
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


route : Location -> Page
route location =
    Radio.Router.urlToPage location.pathname


init : Location -> ( Radio.Model.Model, Cmd Msg )
init location =
    let
        model =
            { tracks = Tracklist.empty
            , radio = Radio.Model.emptyPlaylist Radio "/api/playlist"
            , showRadioPlaylist = False
            , latestTracks = Radio.Model.emptyPlaylist LatestTracks "/api/latest-tracks"
            , played = []
            , playing = False
            , currentPage = route location
            , lastKeyPressed = Nothing
            , currentTime = Nothing
            , player = Player.initialize [ Radio, LatestTracks ]
            , navigation = navigation
            }

        navigateToLocation =
            Update.update (NavigateTo (route location))

        initializeRadio =
            Http.send (FetchedMore Radio (RadioPage == route location)) (Api.fetchPlaylist "/public/json/playlist.json" Api.decodeTrack)

        fetchLatestTracks =
            Update.update (FetchMore LatestTracks False)

        setCurrentTime =
            Task.perform UpdateCurrentTime Time.now
    in
    model
        |> navigateToLocation
        |> andThen fetchLatestTracks
        |> addCmd initializeRadio
        |> addCmd setCurrentTime



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
    , Model.NavigationItem "New" "/latest" LatestTracksPage (Just LatestTracks)
    ]
