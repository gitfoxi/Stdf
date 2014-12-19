
-- TODO: Instead of failing pin indecies, dump the failing pin names -- maybe in a second processing step for separation of concerns
-- TODO: "Returned states" would look better as a string than a long ordered list. I'd also like an option to supress them since 93k doesn't use.
-- TODO: Some things use Label where TestSuite would make more sense: Ftr, Ptr, Mpr should be consistent with the names.
-- TODO: Some tests
-- TODO: Xml output
-- TODO: JSON/XSD data model for STDF XTDF!
-- TODO: Recover to next record with warning if get runs out of data
-- TODO: make sure to cover all "is optional if it is the last field in the record
--       or find some automatic way to recover from end of input mid-record


module Data.Stdf ( parse
                 , parseFile
                 , Stdf(..)
                 , Rec(..)
                 , OptionalInfo(..)
                 , TestFlag(..)
                 ) where

import Data.Stdf.Types
import Data.Stdf.Parser

