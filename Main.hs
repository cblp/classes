{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main, Month (..)) where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Default               (def)
import           Data.Hashable              (Hashable, hash)
import qualified Data.Map                   as Map
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as Text
import           Data.Time                  (Day (..), LocalTime (..),
                                             TimeOfDay (..), fromGregorian)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           GHC.Generics               (Generic)
import           Text.ICalendar             (DTEnd (..), DTStamp (..),
                                             DTStart (..), DateTime (..),
                                             Summary (..), UID (..),
                                             VCalendar (..), VEvent (..),
                                             printICalendar)

main :: IO ()
main = BS.putStrLn $
    printICalendar def def{vcEvents = Map.fromList $ map classEvent classes}

deriving instance Generic Day
deriving instance Generic TimeOfDay
deriving instance Hashable Day
deriving instance Hashable TimeOfDay

data Class = Class
    {day :: Day, subject :: Text, timeEnd :: TimeOfDay, timeStart :: TimeOfDay}
    deriving (Generic, Hashable, Show)

classes :: [Class]
classes =
    [ Class
        { day = fromGregorian 2017 (monthNumber m) d
        , timeStart = TimeOfDay 18 30 0
        , timeEnd = TimeOfDay 21 30 0
        , subject =
            "Философские вопросы естествознания, социальных и гуманитарных наук"
        }
    | m :- d <-
        [ Nov :- 1, Nov :- 16, Nov :- 30
        , Dec :- 13, Dec :- 14, Dec :- 27, Dec :- 28
        ]
    ]

classEvent :: Class -> ((Text, Maybe a), VEvent)
classEvent cls@Class{day, subject, timeEnd, timeStart} = ((uid, Nothing), event)
  where
    uid = Text.pack $ show $ hash cls
    event = VEvent
        { veDTStamp = DTStamp
            {dtStampValue = posixSecondsToUTCTime 0, dtStampOther = def}
        , veUID = UID{uidValue = uid, uidOther = def}
        , veDTStart = Just DTStartDateTime
            {dtStartDateTimeValue = dateTime day timeStart, dtStartOther = def}
        , veDTEndDuration = Just $ Left DTEndDateTime
            {dtEndDateTimeValue = dateTime day timeEnd, dtEndOther = def}
        , veSummary = Just Summary
            { summaryValue = subject
            , summaryAltRep = def
            , summaryLanguage = def
            , summaryOther = def
            }
        , veClass = def
        , veCreated = def
        , veDescription = def
        , veGeo = def
        , veLastMod = def
        , veLocation = def
        , veOrganizer = def
        , vePriority = def
        , veSeq = def
        , veStatus = def
        , veTransp = def
        , veUrl = def
        , veRecurId = def
        , veRRule = def
        , veAttach = def
        , veAttendee = def
        , veCategories = def
        , veComment = def
        , veContact = def
        , veExDate = def
        , veRStatus = def
        , veRelated = def
        , veResources = def
        , veRDate = def
        , veAlarms = def
        , veOther = def
        }
    dateTime localDay localTimeOfDay = ZonedDateTime
        { dateTimeFloating = LocalTime{localDay, localTimeOfDay}
        , dateTimeZone = "MSK"
        }

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)

data Month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Enum)

monthNumber :: Month -> Int
monthNumber = succ . fromEnum
