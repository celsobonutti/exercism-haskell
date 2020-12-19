{-# LANGUAGE NamedFieldPuns #-}

module Clock
  ( addDelta,
    fromHourMin,
    toString,
  )
where

import Text.Printf

data Clock = Clock {hour, minute :: Int}
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute =
  Clock
    { hour = normalizedHour,
      minute = normalizedMinute
    }
  where
    normalizedMinute = minute `mod` 60
    normalizedHour = (hour + minute `div` 60) `mod` 24

toString :: Clock -> String
toString Clock {hour, minute} = printf "%.2d:%.2d" hour minute

addDelta :: Int -> Int -> Clock -> Clock
addDelta hourToAdd minuteToAdd Clock {hour, minute} =
  fromHourMin newHour newMinute
  where
    newHour = hour + hourToAdd
    newMinute = minute + minuteToAdd
