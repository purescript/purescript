module JQuery where

  -- The jQuery wrapper type
  foreign import data JQuery :: *

  -- Wrapper function for jQuery selection $('..')
  foreign import select :: String -> JQuery

  -- Wrapper function for jQuery creation e.g. $('<div>')
  foreign import create :: String -> JQuery

  -- .attr({ ... })
  foreign import attr :: forall attr. JQuery -> { | attr } -> JQuery

  -- .css({ ... })
  foreign import css :: forall css. JQuery -> { | css } -> JQuery

  -- .append(...)
  foreign import append :: JQuery -> JQuery -> JQuery

  -- .appendTo(...)
  foreign import appendTo :: JQuery -> JQuery -> JQuery

  -- .append(...)
  foreign import appendText :: JQuery -> String -> JQuery
