module PhotoGrooveTests exposing (..)

import Expect
import Fuzz exposing (int, string)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Msg(..), initialModel, update)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (undefined)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(não foi achado)")


slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)
