{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main, Month (..)) where

import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.Default                   (def)
import           Data.Hashable                  (Hashable, hash)
import qualified Data.Map                       as Map
import           Data.Semigroup                 ((<>))
import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as Text
import           Data.Time                      (Day (..), LocalTime (..),
                                                 TimeOfDay (..), fromGregorian)
import           Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import           Data.Time.Clock.POSIX          (posixSecondsToUTCTime)
import           GHC.Generics                   (Generic)
import           Text.ICalendar                 (DTEnd (..), DTStamp (..),
                                                 DTStart (..), DateTime (..),
                                                 Description (..),
                                                 Location (..), Summary (..),
                                                 UID (..), VCalendar (..),
                                                 VEvent (..), printICalendar)

main :: IO ()
main = BS.putStrLn $
    printICalendar def def{vcEvents = Map.fromList $ map classEvent classes}

deriving instance Generic Day
deriving instance Generic TimeOfDay
deriving instance Hashable Day
deriving instance Hashable TimeOfDay

data Class = Class
    { address   :: Text
    , day       :: Day
    , room      :: Text
    , subject   :: Text
    , teacher   :: Text
    , timeEnd   :: TimeOfDay
    , timeStart :: TimeOfDay
    }
    deriving (Generic, Hashable, Show)

classes :: [Class]
classes = concat
    [   [ Class
            { day
            , timeStart = timeOfDay (18, 30)
            , timeEnd = timeOfDay (21, 30)
            , subject =
                "Философские вопросы естествознания, социальных и гуманитарных наук"
            , teacher = "Петруня О. Э."
            , room = case weekDay day of
                3  -> "319А"
                4  -> "318А"
                wd -> error $ show wd
            , address = "м. Молодёжная"
            }
        | m :- d <-
            [ Nov :- 01, Nov :- 16, Nov :- 30
            , Dec :- 13, Dec :- 14, Dec :- 27, Dec :- 28
            ]
        , let day = fromGregorian 2017 (monthNumber m) d
        ]
    ,   [ Class
            { day = fromGregorian 2017 (monthNumber m) d
            , timeStart = timeOfDay start
            , timeEnd = timeOfDay end
            , subject = "История и методология информатики"
            , teacher = "Семенов Г. А."
            , room = "623 или 624"
            , address = "м. Таганская, Берниковская наб., 14"
            }
        | m :- d :- (start, end) <-
            [ Nov :- 25 :- (16 :- 30, 21 :- 30)
            , Dec :- 16 :- (16 :- 30, 19 :- 45)
            , Dec :- 23 :- (16 :- 30, 21 :- 30)
            , Dec :- 30 :- (09 :- 00, 14 :- 30)
            ]
        ]
    ,   [ Class
            { day = fromGregorian 2017 (monthNumber m) d
            , timeStart = timeOfDay start
            , timeEnd = timeOfDay end
            , subject = "Компьютерные технологии в науке и образовании"
            , teacher = "Семенов Г. А."
            , room = "623 или 624"
            , address = "м. Таганская, Берниковская наб., 14"
            }
        | m :- d :- (start, end) <-
            [ Nov :- 11 :- (16 :- 30, 21 :- 30)
            , Dec :- 16 :- (20 :- 00, 21 :- 35)
            ]
        ]
    ]

classEvent :: Class -> ((Text, Maybe a), VEvent)
classEvent cls@Class{address, day, room, subject, teacher, timeEnd, timeStart} =
    uid :- Nothing :- event
  where
    uid = Text.pack $ show $ hash cls
    event = VEvent
        { veDTStamp =
            DTStamp{dtStampValue = posixSecondsToUTCTime 0, dtStampOther = def}
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
        , veDescription = Just Description
            { descriptionValue = "Преподаватель — " <> teacher
            , descriptionAltRep = def
            , descriptionLanguage = def
            , descriptionOther = def
            }
        , veLocation = Just Location
            { locationValue = address <> ", аудитория " <> room
            , locationAltRep = def
            , locationLanguage = def
            , locationOther = def
            }
        , veClass = def
        , veCreated = def
        , veGeo = def
        , veLastMod = def
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
    dateTime localDay localTimeOfDay =
        FloatingDateTime{dateTimeFloating = LocalTime{localDay, localTimeOfDay}}

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)

data Month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Enum)

monthNumber :: Month -> Int
monthNumber = succ . fromEnum

weekDay :: Day -> Int
weekDay = snd . mondayStartWeek

timeOfDay :: (Int, Int) -> TimeOfDay
timeOfDay (hh, mm) = TimeOfDay hh mm 00
