module Tests exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (Decoder, null, string)
import Json.Decode.Pipeline.Strict
    exposing
        ( decode
        , end
        , optional
        , optionalAt
        , required
        , requiredAt
        , resolve
        )
import Test exposing (..)


{-| Run some JSON through a Decoder and return the result.
-}
runWith : String -> Decoder a -> Result String a
runWith =
    flip Json.Decode.decodeString


isError : Result err ok -> Bool
isError result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


expectErr : Result err ok -> Expectation
expectErr result =
    isError result
        |> Expect.true ("Expected an Err but got " ++ toString result)


all : Test
all =
    describe
        "Json.Decode.Pipeline"
        [ test "should fail with extra key" <|
            \() ->
                decode (,)
                    |> required "a" string
                    |> required "b" string
                    |> end
                    |> runWith """{"a":"foo","b":"bar","c":"baz"}"""
                    |> Expect.equal (Err "I ran into a `fail` decoder: c")
        , test "should succeed with no extra key" <|
            \() ->
                decode (,)
                    |> required "a" string
                    |> required "b" string
                    |> end
                    |> runWith """{"a":"foo","b":"bar"}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should succeed with optional key which does not exist" <|
            \() ->
                decode (,)
                    |> required "a" string
                    |> optional "b" string "bob"
                    |> end
                    |> runWith """{"a":"foo"}"""
                    |> Expect.equal (Ok ( "foo", "bob" ))
        , test "should succeed with optional key which exists" <|
            \() ->
                decode (,)
                    |> required "a" string
                    |> optional "b" string "bob"
                    |> end
                    |> runWith """{"a":"foo","b":"bar"}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode requiredAt fields" <|
            \() ->
                decode (,)
                    |> requiredAt [ "a" ] string
                    |> requiredAt [ "b", "c" ] string
                    |> end
                    |> runWith """{"a":"foo","b":{"c":"bar"}}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode optionalAt fields" <|
            \() ->
                decode (,)
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> end
                    |> runWith """{"a":{},"x":{"y":"bar"}}"""
                    |> Expect.equal (Ok ( "--", "bar" ))
        , test "optional succeeds if the field is not present" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> end
                    |> runWith """{"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "optional succeeds with fallback if the field is present but null" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> end
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "optional succeeds with result of the given decoder if the field is null and the decoder decodes nulls" <|
            \() ->
                decode (,)
                    |> optional "a" (null "null") "--"
                    |> optional "x" string "--"
                    |> end
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "null", "five" ))
        , test "optional fails if the field is present but doesn't decode" <|
            \() ->
                decode (,)
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> end
                    |> runWith """{"x":5}"""
                    |> expectErr
        , test "optionalAt fails if the field is present but doesn't decode" <|
            \() ->
                decode (,)
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> end
                    |> runWith """{"a":{},"x":{"y":5}}"""
                    |> expectErr
        ]
