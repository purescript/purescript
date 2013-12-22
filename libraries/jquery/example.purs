main = {
    var div = create "<div>";
    div = div `appendText` "Hello World!" `css` { color: "#FF0000" };
    div = div `appendTo` select "body";
    return { };
  }
