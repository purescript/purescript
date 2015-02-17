module Main where

foreign import data T :: *

foreign import data C :: *

foreign import t "var t = null;" :: T

foreign import inst """
  var inst = {
    show: function(t) {
      return 'Done';
    }
  }""" :: C

foreign import instance inst :: Show T

main = Debug.Trace.print t
