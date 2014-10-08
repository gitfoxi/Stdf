Data.Stdf
=========

Structured Test Data Format (STDF) is used to log test data by semiconductor automated test equipment (ATE).

There's a couple of good, open-source libraries for parsing STDF, notably [PyStdf](https://github.com/cmars/pystdf).


The Data.Stdf advantage
-----------------------

1. Haskell
2. Fast


Install
-------

    cabal install stdf -- Is a TODO. Just clone and build the package

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


Now the Bad News
----------------

Data.Stdf currently only parses the PRR record because that's all I needed.
Well, the code is short and sweet so it won't be too much work to add support
for some more interesting records. I can't think of why I'd bother with all of
the rarely-used uninteresting ones. But maybe if I'm bored.
