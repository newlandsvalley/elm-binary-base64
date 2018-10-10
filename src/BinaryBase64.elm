module BinaryBase64 exposing
    ( Octet, ByteString
    , encode, decode
    )

{-| Library for encoding Binary to Base64 and vice-versa,


# Definition


# Data Types

@docs Octet, ByteString


# Functions

@docs encode, decode

-}

import Array exposing (..)
import Bitwise exposing (..)
import Char exposing (fromCode, toCode)
import List.Extra
import Maybe exposing (withDefault)
import String exposing (fromList, toList)


{-| an 8-bit word masquerading as an Int - we don't have an elm Byte type yet
-}
type alias Octet =
    Int


{-| a ByteString - a list of Octets
-}
type alias ByteString =
    List Octet


encodeArray : Array Char
encodeArray =
    Array.fromList
        [ 'A'
        , 'B'
        , 'C'
        , 'D'
        , 'E'
        , 'F'
        , 'G'
        , 'H'
        , 'I'
        , 'J'
        , 'K'
        , 'L'
        , 'M'
        , 'N'
        , 'O'
        , 'P'
        , 'Q'
        , 'R'
        , 'S'
        , 'T'
        , 'U'
        , 'V'
        , 'W'
        , 'X'
        , 'Y'
        , 'Z'
        , 'a'
        , 'b'
        , 'c'
        , 'd'
        , 'e'
        , 'f'
        , 'g'
        , 'h'
        , 'i'
        , 'j'
        , 'k'
        , 'l'
        , 'm'
        , 'n'
        , 'o'
        , 'p'
        , 'q'
        , 'r'
        , 's'
        , 't'
        , 'u'
        , 'v'
        , 'w'
        , 'x'
        , 'y'
        , 'z'
        , '0'
        , '1'
        , '2'
        , '3'
        , '4'
        , '5'
        , '6'
        , '7'
        , '8'
        , '9'
        , '+'
        , '/'
        ]


int4_char3 : List Int -> List Char
int4_char3 is =
    let
        groupsOfFour values =
            let
                grouper elem acc =
                    case acc of
                        [] ->
                            [ [ elem ] ]

                        x :: xs ->
                            if List.length x == 4 then
                                [ elem ] :: acc

                            else
                                (x ++ [ elem ]) :: xs
            in
            List.reverse <| List.foldl grouper [] values
    in
    is
        |> groupsOfFour
        |> List.map
            (\subgroup ->
                case subgroup of
                    a :: b :: c :: d :: t ->
                        let
                            n =
                                Bitwise.or
                                    (Bitwise.or (Bitwise.shiftLeftBy 18 a) (Bitwise.shiftLeftBy 12 b))
                                    (Bitwise.or (Bitwise.shiftLeftBy 6 c) d)
                        in
                        fromCode (Bitwise.and (Bitwise.shiftRightBy 16 n) 0xFF)
                            :: fromCode (Bitwise.and (Bitwise.shiftRightBy 8 n) 0xFF)
                            :: fromCode (Bitwise.and n 0xFF)
                            :: []

                    [ a, b, c ] ->
                        let
                            n =
                                Bitwise.or
                                    (Bitwise.or
                                        (Bitwise.shiftLeftBy 18 a)
                                        (Bitwise.shiftLeftBy 12 b)
                                    )
                                    (Bitwise.shiftLeftBy 6 c)
                        in
                        [ fromCode (Bitwise.and (Bitwise.shiftRightBy 16 n) 0xFF)
                        , fromCode (Bitwise.and (Bitwise.shiftRightBy 8 n) 0xFF)
                        ]

                    [ a, b ] ->
                        let
                            n =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 18 a)
                                    (Bitwise.shiftLeftBy 12 b)
                        in
                        [ fromCode (Bitwise.and (Bitwise.shiftRightBy 16 n) 0xFF) ]

                    [ _ ] ->
                        []

                    [] ->
                        []
            )
        |> List.concat


char3_int4 : List Char -> List Int
char3_int4 cs =
    char3_int4_fold cs []


char3_int4_fold : List Char -> List Int -> List Int
char3_int4_fold cs acc =
    case cs of
        a :: b :: c :: t ->
            let
                n =
                    Bitwise.or (toCode c) <|
                        Bitwise.or
                            (Bitwise.shiftLeftBy 16 <| toCode a)
                            (Bitwise.shiftLeftBy 8 <| toCode b)

                --    (toCode a `shiftLeft` 16) `or` (toCode b `shiftLeft` 8) `or` (toCode c)
            in
            char3_int4_fold t
                (acc
                    ++ [ Bitwise.and (Bitwise.shiftRightBy 18 n) 0x3F
                       , Bitwise.and (Bitwise.shiftRightBy 12 n) 0x3F
                       , Bitwise.and (Bitwise.shiftRightBy 6 n) 0x3F
                       , Bitwise.and n 0x3F
                       ]
                )

        --   (n `shiftRight` 18 `and` 0x3F) :: (n `shiftRight` 12 `and` 0x3F) :: (n `shiftRight` 6 `and` 0x3F) :: (n `and` 0x3F) :: char3_int4 t
        [ a, b ] ->
            let
                n =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 16 <| toCode a)
                        (Bitwise.shiftLeftBy 8 <| toCode b)

                --   (toCode a `shiftLeft` 16) `or` (toCode b `shiftLeft` 8)
            in
            acc
                ++ [ Bitwise.and (shiftRightBy 18 n) 0x3F
                   , Bitwise.and (shiftRightBy 12 n) 0x3F
                   , Bitwise.and (shiftRightBy 6 n) 0x3F
                   ]

        {-
           [ (n `shiftRight` 18 `and` 0x3F)
           , (n `shiftRight` 12 `and` 0x3F)
           , (n `shiftRight` 6 `and` 0x3F)
           ]
        -}
        [ a ] ->
            let
                n =
                    shiftLeftBy 16 (toCode a)

                --   (toCode a `shiftLeft` 16)
            in
            acc
                ++ [ Bitwise.and (shiftRightBy 18 n) 0x3F
                   , Bitwise.and (shiftRightBy 12 n) 0x3F
                   ]

        {-
           [ (n `shiftRight` 18 `and` 0x3F)
           , (n `shiftRight` 12 `and` 0x3F)
           ]
        -}
        [] ->
            acc



-- Retrieve base64 char, given an array index integer in the range [0..63]


enc1 : Int -> Char
enc1 i =
    Array.get i encodeArray
        |> withDefault '?'



-- Pad a base64 code to a multiple of 4 characters, using the special
-- '=' character.


quadruplets : List Char -> List Char
quadruplets cs =
    quadruplets_fold cs []


quadruplets_fold : List Char -> List Char -> List Char
quadruplets_fold cs acc =
    case cs of
        a :: b :: c :: d :: t ->
            quadruplets_fold t (acc ++ [ a, b, c, d ])

        [ a, b, c ] ->
            acc ++ [ a, b, c, '=' ]

        -- 16bit tail unit
        [ a, b ] ->
            acc ++ [ a, b, '=', '=' ]

        -- 8bit tail unit
        [ _ ] ->
            []

        [] ->
            acc



-- 24bit tail unit
-- worker that encodes and then pads to a quadruplet multiple


enc : List Int -> List Char
enc =
    List.map enc1
        >> quadruplets



-- decode worker


dcd : List Char -> List Int
dcd cs =
    cs
        |> List.Extra.takeWhile (\a -> a /= '=')
        |> List.map
            (\h ->
                if h <= 'Z' && h >= 'A' then
                    toCode h - toCode 'A'

                else if h >= '0' && h <= '9' then
                    toCode h - toCode '0' + 52

                else if h >= 'a' && h <= 'z' then
                    toCode h - toCode 'a' + 26

                else if h == '+' then
                    62

                else if h == '/' then
                    63

                else
                    -- This case can't happen since we only call this function if isBase64 is true.
                    0
            )


isBase64Char : Char -> Bool
isBase64Char c =
    (List.member c <| Array.toList encodeArray) || (c == '=')


isBase64 : String -> Bool
isBase64 =
    String.all isBase64Char



-- top level decoding


decodeString : String -> ByteString
decodeString =
    -- log "decode buffer" >>
    String.toList
        >> dcd
        >> int4_char3
        >> List.map toCode



-- Exported functions.


{-| encode a ByteString as Base64
-}
encode : ByteString -> String
encode =
    -- log "encode buffer" >>
    List.map fromCode
        >> char3_int4
        >> enc
        >> String.fromList


{-| decode a Base64 String
-}
decode : String -> Result String ByteString
decode s =
    if isBase64 s then
        -- Ok <| log "decoded buffer" (decodeString s)
        Ok (decodeString s)

    else
        Err "invalid Base64 string"
