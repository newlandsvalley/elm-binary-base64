elm-binary-base64
=================
[![Build Status](https://travis-ci.org/newlandsvalley/elm-binary-base64.svg?branch=master)](https://travis-ci.org/newlandsvalley/elm-binary-base64)

Yet another Base64 implementation (there were over 1000 of these in various languages hosted on GitHub when last I looked).  This one's in elm 0.18, and concentrates on encoding (and then decoding) binary data. I was hoping that a byte array data type would be introduced as long ago as 0.17 but unfortunately it didn't happen - this means that this project is not a great deal of use until elm has a story to tell. Code derived from [here](https://searchcode.com/codesearch/raw/19162450/).

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

cd to example and run:

    ./compile.sh

To test
-------

simply run:

    elm-test

Limitations
-----------

I was originally intending not to publish this package but was persuaded to do so because a couple of people have found use for it.  If you only need to encode, then it is probably useful as it stands.  If you need to decode, then you will probably have to drop down to JavaScript in order to convert the result into some kind of byte array.  

There's not a lot of point in decoding Base64 and then just providing a List of Ints - we really need a Byte array buffer of some sort which we can then (say) pass to javascript APIs that require them.  However, it will be very simple to upgrade this library if and when such facilities are available.  See [this issue](https://groups.google.com/forum/#!topic/elm-discuss/spr621OlUeo).
