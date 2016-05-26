elm-binary-base64
=================

Yet another Base64 implementation (there were over 1000 of these in various languages hosted on GitHub when last I looked).  This one's in elm 0.17, and concentrates on encoding (and then decoding) binary data. I was hoping that a byte array data type would be introduced in 0.17 but unfortunately it didn't happen - this means that this project is of little use until elm has a story to tell. Code derived from [here](https://searchcode.com/codesearch/raw/19162450/).

It defines the following data types:

    {-| a Byte masquerading as an Int constrained to be within the range 0-255 -}
    type alias Octet = Int
    
    {-| a ByteString - a list of Octets -}
    type alias ByteString = List Octet
    
and the following functions:

    encode : ByteString -> String
    
    decode : String -> Result String ByteString
    
To build example
----------------

cd to test and run

    ./compile.sh 
    
To test
-------

cd to test and run

    ./test.sh    
    
Limitations
-----------

There's not a lot of point in decoding Base64 and then just providing a List of Ints - we really need a Byte array buffer of some sort which we can then (say) pass to javascript APIs that require them.  However, it will be very simple to upgrade this library if and when such facilities are available.  See [this issue](https://groups.google.com/forum/#!topic/elm-discuss/spr621OlUeo).