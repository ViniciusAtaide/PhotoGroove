module PhotoGrooveTests exposing (..)

import Expect
import Fuzz exposing (int, string)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove
import Test exposing (..)


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
                |> Expect.equal (Ok "(untitled)")


slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            Expect.equal amount 1