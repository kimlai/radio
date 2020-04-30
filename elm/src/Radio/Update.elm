module Radio.Update exposing (..)

import Api
import Char
import Http exposing (Error(..))
import Json.Decode
import Keyboard
import Navigation
import Player
import PlayerEngine
import Radio.Model as Model exposing (Model, Page(..), PlaylistId(..), PlaylistStatus(..))
import Radio.Ports as Ports
import Radio.Router
import Task exposing (Task)
import Track exposing (StreamingInfo(..), Track, TrackId)
import Tracklist
import Update


type Msg
    = TogglePlayback
    | Next
    | TrackProgress ( TrackId, Float, Float )
    | Play
    | Pause
    | TrackError TrackId
    | FastForward
    | Rewind
    | FollowLink String
    | NavigateTo Page
    | KeyPressed Keyboard.KeyCode
    | PlayFromPlaylist PlaylistId Int
    | PlayOutsidePlaylist TrackId
    | FetchMore PlaylistId Bool
    | FetchedMore PlaylistId Bool (Result Http.Error ( List Track, Maybe String ))
    | ResumeRadio
    | SeekTo Float
    | ToggleRadioPlaylist


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FetchedMore playlistId autoplay (Ok ( tracks, nextLink )) ->
            let
                nextTrackIndex =
                    model.player
                        |> Player.playlistContent playlistId
                        |> List.length

                updatePlaylist playlist =
                    { playlist
                        | nextLink = nextLink
                        , status = Fetched
                    }

                updatedModel =
                    { model
                        | tracks = Tracklist.add tracks model.tracks
                        , player = Player.appendTracksToPlaylist playlistId (List.map .id tracks) model.player
                    }

                newModel =
                    case playlistId of
                        Radio ->
                            ( { updatedModel | radio = updatePlaylist model.radio }
                            , Cmd.none
                            )

                        LatestTracks ->
                            ( { updatedModel | latestTracks = updatePlaylist model.latestTracks }
                            , Cmd.none
                            )
            in
            newModel
                |> Update.when (always autoplay) (update (PlayFromPlaylist playlistId nextTrackIndex))

        FetchedMore playlistId autoplay (Err error) ->
            ( model, Cmd.none )

        PlayFromPlaylist playlistId position ->
            let
                player =
                    Player.select playlistId position model.player

                msg =
                    if Player.currentTrack player == Player.currentTrack model.player then
                        TogglePlayback

                    else
                        Play
            in
            update
                msg
                { model | player = player }

        PlayOutsidePlaylist trackId ->
            let
                player =
                    Player.selectOutsidePlaylist trackId model.player

                msg =
                    if Player.currentTrack player == Player.currentTrack model.player then
                        TogglePlayback

                    else
                        Play
            in
            update
                msg
                { model | player = player }

        FetchMore playlistId autoplay ->
            let
                markAsFetching playlist =
                    { playlist | status = Fetching }

                ( playlist, updateModel ) =
                    case playlistId of
                        Radio ->
                            ( model.radio, \model fn -> { model | radio = fn model.radio } )

                        LatestTracks ->
                            ( model.latestTracks, \model fn -> { model | latestTracks = fn model.latestTracks } )
            in
            case playlist.nextLink of
                Nothing ->
                    ( model, Cmd.none )

                Just url ->
                    ( updateModel model markAsFetching
                    , Http.send (FetchedMore playlistId autoplay) (Api.fetchPlaylist url Api.decodeTrack)
                    )

        Play ->
            case Model.currentTrack model of
                Nothing ->
                    ( model
                    , PlayerEngine.pause Nothing
                    )

                Just track ->
                    let
                        resetTrack track =
                            if track.progress > 99.9 then
                                { track | currentTime = 0, progress = 0 }

                            else
                                track

                        updatePlayedTracks tracks =
                            if List.head tracks /= Just track.id && track.progress == 0 then
                                track.id :: tracks

                            else
                                tracks

                        updatedModel =
                            { model
                                | tracks = Tracklist.update track.id resetTrack model.tracks
                                , played = updatePlayedTracks model.played
                            }
                    in
                    ( { updatedModel | playing = True }
                    , PlayerEngine.play (resetTrack track)
                    )

        Pause ->
            ( { model | playing = False }
            , PlayerEngine.pause (Player.currentTrack model.player)
            )

        ResumeRadio ->
            update Play { model | player = Player.selectPlaylist Radio model.player }

        TrackError trackId ->
            let
                newModel =
                    { model | tracks = Tracklist.update trackId Track.markAsErrored model.tracks }

                ( newModelWithNext, command ) =
                    update Next newModel
            in
            newModelWithNext ! [ command ]

        TogglePlayback ->
            if model.playing then
                update Pause model

            else
                update Play model

        Next ->
            let
                currentPlaylist =
                    Player.currentPlaylist model.player

                nextTrack =
                    (Player.currentTrack << Player.next) model.player
            in
            case nextTrack of
                Just track ->
                    update Play { model | player = Player.next model.player }

                Nothing ->
                    case currentPlaylist of
                        Nothing ->
                            ( model, Cmd.none )

                        Just playlistId ->
                            update (FetchMore playlistId True) model

        TrackProgress ( trackId, progress, currentTime ) ->
            ( { model
                | tracks = Tracklist.update trackId (Track.recordProgress progress currentTime) model.tracks
              }
            , Cmd.none
            )

        NavigateTo page ->
            ( { model | currentPage = page }, Cmd.none )

        FollowLink url ->
            ( model
            , Navigation.newUrl url
            )

        SeekTo positionInPercentage ->
            ( model
            , PlayerEngine.seekToPercentage (Model.currentTrack model) positionInPercentage
            )

        FastForward ->
            ( model
            , PlayerEngine.changeCurrentTime (Model.currentTrack model) 10
            )

        Rewind ->
            ( model
            , PlayerEngine.changeCurrentTime (Model.currentTrack model) -10
            )

        ToggleRadioPlaylist ->
            ( { model | showRadioPlaylist = not model.showRadioPlaylist }
            , Cmd.none
            )

        KeyPressed keyCode ->
            case Char.fromCode keyCode of
                'n' ->
                    update Next model

                'p' ->
                    update TogglePlayback model

                'l' ->
                    update FastForward model

                'h' ->
                    update Rewind model

                'j' ->
                    ( model
                    , Ports.scroll 120
                    )

                'k' ->
                    ( model
                    , Ports.scroll -120
                    )

                'g' ->
                    if model.lastKeyPressed == Just 'g' then
                        ( { model | lastKeyPressed = Nothing }
                        , Ports.scroll -9999999
                        )

                    else
                        ( { model | lastKeyPressed = Just 'g' }
                        , Cmd.none
                        )

                'G' ->
                    ( model
                    , Ports.scroll 99999999
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )
