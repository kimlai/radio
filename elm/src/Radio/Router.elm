module Radio.Router exposing (..)

import Radio.Model exposing (Page(..))
import Url exposing (Url)


urlToPage : Url -> Page
urlToPage { path } =
    case path of
        "/" ->
            RadioPage

        "/queue/played" ->
            PlayedPage

        "/queue/next" ->
            UpNextPage

        "/latest" ->
            LatestTracksPage

        _ ->
            PageNotFound
