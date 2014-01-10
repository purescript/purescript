module JQuery where

  import Prelude

  -- An effect type which indicates DOM manipulation
  foreign import data DOM :: !

  -- The jQuery wrapper type
  foreign import data JQuery :: *

  -- Wrapper function for jQuery selection $('..')
  foreign import select "function select(selector) { return function () { return jQuery(selector); }; }" :: forall eff. String -> Eff (dom :: DOM | eff) JQuery

  -- Wrapper function for jQuery creation e.g. $('<div>')
  foreign import create "function create(html) { return function () { return jQuery(html); }; }" :: forall eff. String -> Eff (dom :: DOM | eff) JQuery

  -- .attr({ ... })
  foreign import attr "function attr(attrs) { return function(ob) { return function () { return ob.attr(attrs); }; }; }" :: forall eff attr. { | attr } -> JQuery -> Eff (dom :: DOM | eff) JQuery

  -- .css({ ... })
  foreign import css "function css(props) { return function(ob) { return function () { return ob.css(props); }; }; }" :: forall eff css. { | css } -> JQuery -> Eff (dom :: DOM | eff) JQuery

  -- .append(...)
  foreign import append "function append(ob) { return function(ob1) { return function () { return ob.append(ob1); }; }; }" :: forall eff. JQuery -> JQuery -> Eff (dom :: DOM | eff) JQuery

  -- .append(...)
  foreign import appendText "function appendText(ob) { return function(s) { return function () { return ob.append(s); }; }; }" :: forall eff. JQuery -> String -> Eff (dom :: DOM | eff) JQuery
  
  -- Get the document body
  foreign import body "function body() { return jQuery(document.body); }" :: forall eff. Eff (dom :: DOM | eff) JQuery
