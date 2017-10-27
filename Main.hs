{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

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
    printICalendar
        def
        def{vcEvents = Map.fromList $ foldMap classEvents theClasses}

deriving instance Generic Day
deriving instance Generic TimeOfDay
deriving instance Hashable Day
deriving instance Hashable TimeOfDay

data ClassSeries = ClassSeries
    { base    :: Text
    , classes :: [Class]
    , subject :: Text
    , teacher :: Text
    }
    deriving (Generic, Hashable)

data Class = Class
    { room     :: Text
    , datetime :: SimpleDate :- (SimpleTime :- SimpleTime)
    }
    deriving (Generic, Hashable)

theClasses :: [ClassSeries]
theClasses =
    [ ClassSeries
        { subject =
            "Философские вопросы естествознания, социальных и гуманитарных наук"
        , teacher = "Петруня О. Э."
        , base = molodyozhnaya
        , classes =
            [ Class
                { room = case dayOfWeek day of
                    3  -> "319А"
                    4  -> "318А"
                    wd -> error $ show wd
                , datetime = day :- (18 :- 30, 21 :- 30)
                }
            | day <-
                [ Nov :- 01, Nov :- 16, Nov :- 30
                , Dec :- 13, Dec :- 14, Dec :- 27, Dec :- 28
                ]
            ]
        }
    , ClassSeries
        { subject = "История и методология информатики"
        , teacher = semenov
        , base = taganskaya
        , classes =
            map (Class "623 или 624")
                [ Nov :- 25 :- (16 :- 30, 21 :- 30)
                , Dec :- 16 :- (16 :- 30, 19 :- 45)
                , Dec :- 23 :- (16 :- 30, 21 :- 30)
                , Dec :- 30 :- (09 :- 00, 14 :- 30)
                ]
        }
    , ClassSeries
        { subject = "Компьютерные технологии в науке и образовании"
        , teacher = semenov
        , base = taganskaya
        , classes =
            map (Class "623 или 624")
                [ Nov :- 11 :- (16 :- 30, 21 :- 30)
                , Dec :- 16 :- (20 :- 00, 21 :- 30)
                ]
        }
    , ClassSeries
        { subject = "Исполнительные механизмы автоматических систем"
        , teacher = "Николаев П. Л."
        , base = molodyozhnaya
        , classes =
            map (Class "411В")
                [ Dec :- 02 :- (14 :- 45, 19 :- 45)
                , Dec :- 09 :- (13 :- 00, 19 :- 45)
                ]
        }
    , ClassSeries
        { subject = "Сетевые проблемно-ориентированные системы (фактически Си)"
        , teacher = "Кейно Павел Петрович"
        , base = molodyozhnaya
        , classes =
            map (Class "413В")
                [ Oct :- 21 :- (14 :- 45, 18 :- 00)
                , Oct :- 28 :- (16 :- 30, 21 :- 30)
                ]
        }
    ]
  where
    molodyozhnaya = "м. Молодёжная"
    taganskaya = "м. Таганская, Берниковская наб., 14"
    semenov = "Семенов Г. А."

classEvents :: ClassSeries -> [Text :- Maybe a :- VEvent]
classEvents series@ClassSeries{base, subject, teacher, classes} =
    [ uidValue (veUID event) :- Nothing :- event
    | datetime <- classes
    , let event = makeEvent datetime
    ]
  where
    makeEvent cls@Class{datetime = (day, (start, end)), room} = VEvent
        { veDTStamp =
            DTStamp{dtStampValue = posixSecondsToUTCTime 0, dtStampOther = def}
        , veUID = UID
            {uidValue = Text.pack . show $ hash (series, cls), uidOther = def}
        , veDTStart = Just DTStartDateTime
            {dtStartDateTimeValue = dateTime start, dtStartOther = def}
        , veDTEndDuration = Just $ Left DTEndDateTime
            {dtEndDateTimeValue = dateTime end, dtEndOther = def}
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
            { locationValue = base <> ", аудитория " <> room
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
      where
        dateTime :: SimpleTime -> DateTime
        dateTime time = FloatingDateTime
            { dateTimeFloating = LocalTime
                {localDay = realDay day, localTimeOfDay = timeOfDay time}
            }

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)

type a :- b = (a, b)

data Month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Enum, Generic, Hashable)

monthNumber :: Month -> Int
monthNumber = succ . fromEnum

dayOfWeek :: SimpleDate -> Int
dayOfWeek = snd . mondayStartWeek . realDay

timeOfDay :: SimpleTime -> TimeOfDay
timeOfDay (hh, mm) = TimeOfDay hh mm 00

realDay :: SimpleDate -> Day
realDay (m, d) = fromGregorian 2017 (monthNumber m) d

type DayOfMonth = Int

type SimpleDate = Month :- DayOfMonth

type SimpleTime = Int :- Int
