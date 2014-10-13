Data.Stdf
=========

Structured Test Data Format (STDF) is used to log test data by semiconductor automated test equipment (ATE).

There's a couple of good, open-source libraries for parsing STDF, notably [PyStdf](https://github.com/cmars/pystdf).

Latest version: 0.2

[![Build Status](https://travis-ci.org/gitfoxi/Stdf.svg)](https://travis-ci.org/gitfoxi/Stdf)

The Data.Stdf advantage
-----------------------

1. Haskell
2. Fast


Install
-------

    cabal install stdf

Or for the latest revision, clone and build from here.

Usage
-----

Here's an example of converting STDF to JSON. (What else would you want to do
with it?)

    import Data.Stdf
    import qualified Data.ByteString.Lazy.Char8 as BL
    import Data.Aeson

    -- unparsed records have type Raw for the time being
    notRaw :: Rec -> Bool
    notRaw (Raw _) = False
    notRaw _       = True

    main = do
        raw <- BL.readFile "test.stdf"
        let recs = parse raw
        let goodRecs = filter notRaw recs
        mapM_ (BL.putStrLn . encode) goodRecs


Included Examples Applications
------------------------------

* WaferMap -- prints a pretty wafermap like this:

```
$ Examples/WaferMap test.stdf
File: test.stdf
.   .   .   .   .   11  11  11  11  11  .   .   .   .   .   .
.   .   .   11  11  11  11  6   1   6   11  11  11  .   .   .
.   .   11  11  1   11  11  11  1   11  1   1   11  11  .   .
.   1   1   1   11  1   1   1   1   6   6   6   1   1   11  .
203 11  11  11  11  11  1   6   11  1   1   11  1   11  11  .
11  11  11  11  1   11  6   1   1   1   11  11  11  11  11  11
11  11  11  11  1   1   11  1   11  6   1   11  1   11  11  11
203 11  11  1   1   11  6   1   11  212 6   1   11  11  11  .
.   200 6   11  1   1   1   1   1   6   11  1   1   11  11  .
.   .   11  11  11  11  1   1   1   11  1   1   1   11  .   .
.   .   .   11  11  1   11  11  1   11  11  11  11  .   .   .
.   .   .   .   .   11  11  11  11  200 212 .   .   .   .   .
```

* StdfToJson -- prints each Stdf record as a one-line JSON object for easy importing into other languages for further processing


Performance
-----------

Performance is very good but can be 50% better if you build with `-O2`.
However compiling takes forever with `-O2`.


Not Implemented
----------------

### Non-Intel floating-point and endianness

It really blows my mind that Stdf expects the parser to support any N CPU bit
encodings. I won't do it.

### Some tricky records and fields

Not yet covering all fields of all Stdf records.
Several interesting optional fields in PTR, MPR and FTR are not yet processed.
