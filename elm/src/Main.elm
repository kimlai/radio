module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Http
import Json.Decode exposing (field)
import Model
import Player
import PlayerEngine
import Radio.Model exposing (Model, Page(..), PlaylistId(..))
import Radio.Router
import Radio.Update as Update exposing (Msg(..))
import Radio.View as View
import Task
import Tracklist
import Update exposing (addCmd, andThen)
import Url exposing (Url)


main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Int -> Url -> Nav.Key -> ( Radio.Model.Model, Cmd Msg )
init playlistId url key =
    let
        playlistUrl =
            "/json/playlists/" ++ String.fromInt playlistId ++ "/page_1.json"

        model =
            { tracks = Tracklist.empty
            , radio = Radio.Model.emptyPlaylist Radio playlistUrl
            , showRadioPlaylist = False
            , latestTracks = Radio.Model.emptyPlaylist LatestTracks "/json/tracks/page_1.json"
            , played = []
            , playing = False
            , currentPage = Radio.Router.urlToPage url
            , player = Player.initialize [ Radio, LatestTracks ]
            , navigation = navigation
            , key = key
            }
    in
    ( model
    , Cmd.batch
        [ Http.get
            { url = playlistUrl
            , expect = Http.expectJson (FetchedMore Radio False) Api.decodePlaylist
            }
        , Http.get
            { url = "json/tracks/page_1.json"
            , expect = Http.expectJson (FetchedMore LatestTracks False) Api.decodePlaylist
            }
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Radio.Model.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PlayerEngine.trackProgress TrackProgress
        , PlayerEngine.trackEnd (\_ -> Next)
        , PlayerEngine.trackError TrackError
        ]


navigation : List (Model.NavigationItem Radio.Model.Page PlaylistId)
navigation =
    [ Model.NavigationItem "Radio" "/" RadioPage (Just Radio)
    , Model.NavigationItem "All tracks" "/latest" LatestTracksPage (Just LatestTracks)
    ]
