module Tests exposing (..)

import Test exposing (..)
import RpnTest exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    RpnTest.tests
