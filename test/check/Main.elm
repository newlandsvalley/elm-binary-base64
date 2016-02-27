import BinaryBase64 exposing (encode, decode)
import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test
import ElmTest

roundTrip : List Int -> List Int
roundTrip l = 
  let 
    decoded = decode <| encode l
  in case decoded of
    Err s -> [-1]
    Ok l -> l

claim_encode_inverse_of_decode =
  claim
    "decoding an encoded list gives back the original"
  `that`
    (\l -> (roundTrip l))
  `is`
    (identity)
  `for`
    list (rangeInt 0 255)

suite_base64 =
  suite "Base64 Suite"
    [ claim_encode_inverse_of_decode
    ]

evidence : Evidence
evidence = quickCheck suite_base64

main = ElmTest.elementRunner (Check.Test.evidenceToTest evidence)

