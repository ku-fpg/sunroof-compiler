
{-# LANGUAGE TypeFamilies #-}

-- | The 'Date' module provides the API for the Javascript @Date@
--   object. The API documentation is mainly taken from 
--   w3schools (<http://www.w3schools.com/jsref/jsref_obj_date.asp>).
--   Deprecated methods are not supported.
module Language.Sunroof.JS.Date
  ( JSDate
  , newDate
  , getHours, getMinutes, getSeconds
  , getDate, getDay, getFullYear
  , getMilliseconds, getMonth
  , getTime, getTimezoneOffset
  , getUTCDate, getUTCDay, getUTCFullYear
  , getUTCHours, getUTCMilliseconds
  , getUTCMinutes, getUTCMonth, getUTCSeconds
  , parseDate
  , setDate, setFullYear, setHours
  , setMilliseconds, setMinutes, setMonth
  , setSeconds, setTime
  , setUTCDate, setUTCFullYear, setUTCHours
  , setUTCMilliseconds, setUTCMinutes, setUTCMonth
  , setUTCSeconds, toDateString
  , toISOString, toJSON
  , toLocaleDateString, toLocaleTimeString
  , toLocaleString, toString
  , toTimeString, toUTCString
  ) where

import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )

import Language.Sunroof.Classes ( Sunroof(..), SunroofArgument(..) )
import Language.Sunroof.Types ( JS, invoke, new, cast )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.String ( JSString )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSDate Type
-- -------------------------------------------------------------

-- | The type of a date object.
newtype JSDate = JSDate JSObject

-- | Show the Javascript.
instance Show JSDate where
  show (JSDate o) = show o

-- | First-class values in Javascript.
instance Sunroof JSDate where
  box = JSDate . box
  unbox (JSDate o) = unbox o

-- | Associated boolean is 'JSBool'.
type instance BooleanOf JSDate = JSBool

-- | Can be returned in branches.
instance IfB JSDate where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSDate where
  (JSDate a) ==* (JSDate b) = a ==* b

-- -------------------------------------------------------------
-- JSDate Methods
-- -------------------------------------------------------------

-- | Creates a new @Date@ object.
--   See: <http://www.w3schools.com/jsref/jsref_obj_date.asp>
newDate :: (SunroofArgument a) => a -> JS t JSDate
newDate args = cast `fmap` new "Date" args

-- | Returns the hour (from 0-23).
--   See: <http://www.w3schools.com/jsref/jsref_gethours.asp>
getHours :: JSDate -> JS t JSNumber
getHours = invoke "getHours" ()

-- | Returns the minutes (from 0-59).
--   See: <http://www.w3schools.com/jsref/jsref_getminutes.asp>
getMinutes :: JSDate -> JS t JSNumber
getMinutes = invoke "getMinutes" ()

-- | Returns the seconds (from 0-59).
--   See: <http://www.w3schools.com/jsref/jsref_getseconds.asp>
getSeconds :: JSDate -> JS t JSNumber
getSeconds = invoke "getSeconds" ()

-- | Returns the day of the month (from 1-31).
--   See: <http://www.w3schools.com/jsref/jsref_getdate.asp>
getDate :: JSDate -> JS t JSNumber
getDate = invoke "getDate" ()

-- | Returns the day of the week (from 0-6).
--   See: <http://www.w3schools.com/jsref/jsref_getday.asp>
getDay :: JSDate -> JS t JSNumber
getDay = invoke "getDay" ()

-- | Returns the year (four digits).
--   See: <http://www.w3schools.com/jsref/jsref_getfullyear.asp>
getFullYear :: JSDate -> JS t JSNumber
getFullYear = invoke "getFullYear" ()

-- | Returns the milliseconds (from 0-999).
--   See: <http://www.w3schools.com/jsref/jsref_getmilliseconds.asp>
getMilliseconds :: JSDate -> JS t JSNumber
getMilliseconds = invoke "getMilliseconds" ()

-- | Returns the month (from 0-11).
--   See: <http://www.w3schools.com/jsref/jsref_getmonth.asp>
getMonth :: JSDate -> JS t JSNumber
getMonth = invoke "getMonth" ()

-- | Returns the number of milliseconds since midnight Jan 1, 1970.
--   See: <http://www.w3schools.com/jsref/jsref_gettime.asp>
getTime :: JSDate -> JS t JSNumber
getTime = invoke "getTime" ()

-- | Returns the time difference between UTC time and local time, in minutes.
--   See: <http://www.w3schools.com/jsref/jsref_gettimezoneoffset.asp>
getTimezoneOffset :: JSDate -> JS t JSNumber
getTimezoneOffset = invoke "getTimezoneOffset" ()

-- | Returns the day of the month, according to universal time (from 1-31).
--   See: <http://www.w3schools.com/jsref/jsref_getutcdate.asp>
getUTCDate :: JSDate -> JS t JSNumber
getUTCDate = invoke "getUTCDate" ()

-- | Returns the day of the week, according to universal time (from 0-6).
--   See: <http://www.w3schools.com/jsref/jsref_getutcday.asp>
getUTCDay :: JSDate -> JS t JSNumber
getUTCDay = invoke "getUTCDay" ()

-- | Returns the year, according to universal time (four digits).
--   See: <http://www.w3schools.com/jsref/jsref_getutcfullyear.asp>
getUTCFullYear :: JSDate -> JS t JSNumber
getUTCFullYear = invoke "getUTCFullYear" ()

-- | Returns the hour, according to universal time (from 0-23).
--   See: <http://www.w3schools.com/jsref/jsref_getutchours.asp>
getUTCHours :: JSDate -> JS t JSNumber
getUTCHours = invoke "getUTCHours" ()

-- | Returns the milliseconds, according to universal time (from 0-999).
--   See: <http://www.w3schools.com/jsref/jsref_getutcmilliseconds.asp>
getUTCMilliseconds :: JSDate -> JS t JSNumber
getUTCMilliseconds = invoke "getUTCMilliseconds" ()

-- | Returns the minutes, according to universal time (from 0-59).
--   See: <http://www.w3schools.com/jsref/jsref_getutcminutes.asp>
getUTCMinutes :: JSDate -> JS t JSNumber
getUTCMinutes = invoke "getUTCMinutes" ()

-- | Returns the month, according to universal time (from 0-11).
--   See: <http://www.w3schools.com/jsref/jsref_getutcmonth.asp>
getUTCMonth :: JSDate -> JS t JSNumber
getUTCMonth = invoke "getUTCMonth" ()

-- | Returns the seconds, according to universal time (from 0-59).
--   See: <http://www.w3schools.com/jsref/jsref_getutcseconds.asp>
getUTCSeconds :: JSDate -> JS t JSNumber
getUTCSeconds = invoke "getUTCSeconds" ()

-- | Parses a date string and returns the number of milliseconds 
--   since midnight of January 1, 1970.
--   See: <http://www.w3schools.com/jsref/jsref_parse.asp>
parseDate :: JSString -> JSDate -> JS t JSNumber
parseDate str = invoke "parse" str

-- | Sets the day of the month of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_setdate.asp>
setDate :: JSNumber -> JSDate -> JS t ()
setDate n = invoke "setDate" n

-- | Sets the year (four digits) of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_setfullyear.asp>
setFullYear :: JSNumber -> JSDate -> JS t ()
setFullYear n = invoke "setFullYear" n

-- | Sets the hour of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_sethours.asp>
setHours :: JSNumber -> JSDate -> JS t ()
setHours n = invoke "setHours" n

-- | Sets the milliseconds of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_setmilliseconds.asp>
setMilliseconds :: JSNumber -> JSDate -> JS t ()
setMilliseconds n = invoke "setMilliseconds" n

-- | Set the minutes of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_setminutes.asp>
setMinutes :: JSNumber -> JSDate -> JS t ()
setMinutes n = invoke "setMinutes" n

-- | Sets the month of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_setmonth.asp>
setMonth :: JSNumber -> JSDate -> JS t ()
setMonth n = invoke "setMonth" n

-- | Sets the seconds of a date object.
--   See: <http://www.w3schools.com/jsref/jsref_setseconds.asp>
setSeconds :: JSNumber -> JSDate -> JS t ()
setSeconds n = invoke "setSeconds" n

-- | Sets a date and time by adding or subtracting a specified number 
--   of milliseconds to/from midnight January 1, 1970.
--   See: <http://www.w3schools.com/jsref/jsref_settime.asp>
setTime :: JSNumber -> JSDate -> JS t ()
setTime n = invoke "setTime" n

-- | Sets the day of the month of a date object, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_setutcdate.asp>
setUTCDate :: JSNumber -> JSDate -> JS t ()
setUTCDate n = invoke "setUTCDate" n

-- | Sets the year of a date object, according to universal time (four digits).
--   See: <http://www.w3schools.com/jsref/jsref_setutcfullyear.asp>
setUTCFullYear :: JSNumber -> JSDate -> JS t ()
setUTCFullYear n = invoke "setUTCFullYear" n

-- | Sets the hour of a date object, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_setutchours.asp>
setUTCHours :: JSNumber -> JSDate -> JS t ()
setUTCHours n = invoke "setUTCHours" n

-- | Sets the milliseconds of a date object, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_setutcmilliseconds.asp>
setUTCMilliseconds :: JSNumber -> JSDate -> JS t ()
setUTCMilliseconds n = invoke "setUTCMilliseconds" n

-- | Set the minutes of a date object, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_setutcminutes.asp>
setUTCMinutes :: JSNumber -> JSDate -> JS t ()
setUTCMinutes n = invoke "setUTCMinutes" n

-- | Sets the month of a date object, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_setutcmonth.asp>
setUTCMonth :: JSNumber -> JSDate -> JS t ()
setUTCMonth n = invoke "setUTCMonth" n

-- | Set the seconds of a date object, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_setutcseconds.asp>
setUTCSeconds :: JSNumber -> JSDate -> JS t ()
setUTCSeconds n = invoke "setUTCSeconds" n

-- | Converts the date portion of a Date object into a readable string.
--   See: <http://www.w3schools.com/jsref/jsref_todatestring.asp>
toDateString :: JSDate -> JS t JSString
toDateString = invoke "toDateString" ()

-- | Returns the date as a string, using the ISO standard.
--   See: <http://www.w3schools.com/jsref/jsref_toisostring.asp>
toISOString :: JSDate -> JS t JSString
toISOString = invoke "toISOString" ()

-- | Returns the date as a string, formated as a JSON date.
--   See: <http://www.w3schools.com/jsref/jsref_tojson.asp>
toJSON :: JSDate -> JS t JSString
toJSON = invoke "toJSON" ()

-- | Returns the date portion of a Date object as a string, 
--   using locale conventions.
--   See: <http://www.w3schools.com/jsref/jsref_tolocaledatestring.asp>
toLocaleDateString :: JSDate -> JS t JSString
toLocaleDateString = invoke "toLocaleDateString" ()

-- | Returns the time portion of a Date object as a string, 
--   using locale conventions.
--   See: <http://www.w3schools.com/jsref/jsref_tolocaletimestring.asp>
toLocaleTimeString :: JSDate -> JS t JSString
toLocaleTimeString = invoke "toLocaleTimeString" ()

-- | Converts a Date object to a string, using locale conventions.
--   See: <http://www.w3schools.com/jsref/jsref_tolocalestring.asp>
toLocaleString :: JSDate -> JS t JSString
toLocaleString = invoke "toLocaleString" ()

-- | Converts a Date object to a string.
--   See: <http://www.w3schools.com/jsref/jsref_tostring_date.asp>
toString :: JSDate -> JS t JSString
toString = invoke "toString" ()

-- | Converts the time portion of a Date object to a string.
--   See: <http://www.w3schools.com/jsref/jsref_totimestring.asp>
toTimeString :: JSDate -> JS t JSString
toTimeString = invoke "toTimeString" ()

-- | Converts a Date object to a string, according to universal time.
--   See: <http://www.w3schools.com/jsref/jsref_toutcstring.asp>
toUTCString :: JSDate -> JS t JSString
toUTCString = invoke "toUTCString" ()

