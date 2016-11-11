module BinaryBase64
    exposing
        ( encode
        , decode
        , Octet
        , ByteString
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
import Maybe exposing (withDefault)
import String exposing (toList, fromList)
import Result exposing (Result)
import Debug exposing (..)


infixl 6 |||


infixl 6 &&&


infixl 7 <<<


infixl 7 >>>


{-| an 8-bit word masquerading as an Int - we don't have an elm Byte type yet
-}
type alias Octet =
    Int


{-| a ByteString - a list of Octets
-}
type alias ByteString =
    List Octet


(<<<) =
    flip Bitwise.shiftLeftBy


(>>>) =
    flip Bitwise.shiftRightBy


(&&&) =
    Bitwise.and


(|||) =
    Bitwise.or


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
    case is of
        a :: b :: c :: d :: t ->
            let
                n =
                    (a <<< 18) ||| (b <<< 12) ||| (c <<< 6) ||| d

                -- (a `shiftLeft` 18) `or` (b `shiftLeft` 12) `or` (c `shiftLeft` 6) `or` d
            in
                (fromCode (n >>> 16 &&& 0xFF))
                    :: (fromCode (n >>> 8 &&& 0xFF))
                    :: (fromCode (n &&& 0xFF))
                    :: int4_char3 t

        {-
           (fromCode (n `shiftRight` 16 `and` 0xFF))
               :: (fromCode (n `shiftRight` 8 `and` 0xFF))
               :: (fromCode (n `and` 0xFF))
               :: int4_char3 t
        -}
        [ a, b, c ] ->
            let
                n =
                    (a <<< 18) ||| (b <<< 12) ||| (c <<< 6)

                -- (a `shiftLeft` 18) `or` (b `shiftLeft` 12)  `or` (c `shiftLeft` 6)
            in
                [ (fromCode (n >>> 16 &&& 0xFF))
                , (fromCode (n >>> 8 &&& 0xFF))
                ]

        {-
           [ (fromCode (n `shiftRight` 16 `and` 0xFF))
           , (fromCode (n `shiftRight` 8 `and` 0xFF))
           ]
        -}
        [ a, b ] ->
            let
                n =
                    (a <<< 18) ||| (b <<< 12)

                --    (a `shiftLeft` 18) `or` (b `shiftLeft` 12)
            in
                [ (fromCode (n >>> 16 &&& 0xFF)) ]

        --    [ (fromCode (n `shiftRight` 16 `and` 0xFF)) ]
        [ _ ] ->
            log "int4_char3: impossible number of Ints." []

        [] ->
            []


char3_int4 : List Char -> List Int
char3_int4 cs =
    case cs of
        a :: b :: c :: t ->
            let
                n =
                    (toCode a <<< 16) ||| (toCode b <<< 8) ||| (toCode c)

                --    (toCode a `shiftLeft` 16) `or` (toCode b `shiftLeft` 8) `or` (toCode c)
            in
                (n >>> 18 &&& 0x3F) :: (n >>> 12 &&& 0x3F) :: (n >>> 6 &&& 0x3F) :: (n &&& 0x3F) :: char3_int4 t

        --   (n `shiftRight` 18 `and` 0x3F) :: (n `shiftRight` 12 `and` 0x3F) :: (n `shiftRight` 6 `and` 0x3F) :: (n `and` 0x3F) :: char3_int4 t
        [ a, b ] ->
            let
                n =
                    (toCode a <<< 16) ||| (toCode b <<< 8)

                --   (toCode a `shiftLeft` 16) `or` (toCode b `shiftLeft` 8)
            in
                [ (n >>> 18 &&& 0x3F)
                , (n >>> 12 &&& 0x3F)
                , (n >>> 6 &&& 0x3F)
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
                    (toCode a <<< 16)

                --   (toCode a `shiftLeft` 16)
            in
                [ (n >>> 18 &&& 0x3F)
                , (n >>> 12 &&& 0x3F)
                ]

        {-
           [ (n `shiftRight` 18 `and` 0x3F)
           , (n `shiftRight` 12 `and` 0x3F)
           ]
        -}
        [] ->
            []



-- Retrieve base64 char, given an array index integer in the range [0..63]


enc1 : Int -> Char
enc1 i =
    Array.get i encodeArray
        |> withDefault '?'



-- Pad a base64 code to a multiple of 4 characters, using the special
-- '=' character.


quadruplets : List Char -> List Char
quadruplets cs =
    case cs of
        a :: b :: c :: d :: t ->
            a :: b :: c :: d :: quadruplets t

        [ a, b, c ] ->
            [ a, b, c, '=' ]

        -- 16bit tail unit
        [ a, b ] ->
            [ a, b, '=', '=' ]

        -- 8bit tail unit
        [ _ ] ->
            log "quadruplets: impossible number of characters." []

        [] ->
            []



-- 24bit tail unit
-- worker that encodes and then pads to a quadruplet multiple


enc : List Int -> List Char
enc =
    List.map enc1
        >> quadruplets



-- decode worker


dcd : List Char -> List Int
dcd cs =
    case cs of
        [] ->
            []

        h :: t ->
            case h of
                '=' ->
                    []

                -- terminate data stream
                _ ->
                    if h <= 'Z' && h >= 'A' then
                        toCode h - toCode 'A' :: dcd t
                    else if h >= '0' && h <= '9' then
                        toCode h - toCode '0' + 52 :: dcd t
                    else if h >= 'a' && h <= 'z' then
                        toCode h - toCode 'a' + 26 :: dcd t
                    else if h == '+' then
                        62 :: dcd t
                    else if h == '/' then
                        63 :: dcd t
                    else
                        dcd t



-- validation


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
        >> (List.map toCode)



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
    if (isBase64 s) then
        -- Ok <| log "decoded buffer" (decodeString s)
        Ok (decodeString s)
    else
        Err "invalid Base64 string"
