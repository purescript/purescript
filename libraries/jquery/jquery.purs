module JQuery where

  import Eff

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
  foreign import append "function append(ob1) { return function(ob) { return function () { return ob.append(ob1); }; }; }" :: forall eff. JQuery -> JQuery -> Eff (dom :: DOM | eff) JQuery

  -- .append(...)
  foreign import appendText "function appendText(s) { return function(ob) { return function () { return ob.append(s); }; }; }" :: forall eff. String -> JQuery -> Eff (dom :: DOM | eff) JQuery
  
  -- Get the document body
  foreign import body "function body() { return jQuery(document.body); }" :: forall eff. Eff (dom :: DOM | eff) JQuery

  -- Get the text content of an element
  foreign import getText "function getText(ob) { return function() { return ob.text(); }; }" :: forall eff. JQuery -> Eff (dom :: DOM | eff) String

  -- Set the text content of an element
  foreign import setText "function setText(text) { return function(ob) { return function() { ob.text(text); }; }; }" :: forall eff. String -> JQuery -> Eff (dom :: DOM | eff) JQuery

  -- Get the value of a text field
  foreign import getValue "function getValue(ob) { return function() { return ob.val(); }; }" :: forall eff. JQuery -> Eff (dom :: DOM | eff) String

  -- Set the value of a text field
  foreign import setValue "function setValue(val) { return function(ob) { return function() { return ob.val(val); }; }; }" :: forall eff. String -> JQuery -> Eff (dom :: DOM | eff) JQuery

  -- Register an event handler
  foreign import on "function on(evt) { return function(act) { return function(ob) { return function() { return ob.on(evt, function() { act(); } ); }; }; }; }" :: forall eff a. String -> Eff eff a -> JQuery -> Eff (dom :: DOM | eff) JQuery
