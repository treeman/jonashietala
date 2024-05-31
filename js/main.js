function connect() {
  conn = new WebSocket("ws://127.0.0.1:8081");

  conn.onopen = () => {
    console.log("Connected to ws");
  };

  conn.onmessage = (event) => {
    const msg = JSON.parse(event.data);

    switch (msg.type) {
      case "Refresh":
        location.reload();
        break;
      //case "PositionPage":
      //  // TODO this doesn't work when we have images.
      //  // I guess we need to do some searching to find the best match...
      //
      //  // https://stackoverflow.com/questions/5007530/how-do-i-scroll-to-an-element-using-javascript#22292000
      //  //
      //  // Search for inner text!
      //  // https://stackoverflow.com/questions/3813294/how-to-get-element-by-innertext
      //
      //  const curr_url = window.location.href
      //    .replace(window.location.origin, "")
      //    .replace(/\/$/, "");
      //
      //  if (msg.url == curr_url) {
      //    const cursor = msg.linenum / msg.linecount;
      //    // const target = window.scrollMaxY * cursor - window.screen.height / 6;
      //    const target = window.scrollMaxY * cursor;
      //    // const target = window.scrollMaxY * cursor - window.screen.height;
      //    window.scrollTo(0, target);
      //
      //    // window.scrollTo(0, 0);
      //    // window.scrollByLines(msg.linenum);
      //    break;
      //  }
      //default:
      //  console.log("Unknown message: ", msg);
    }

    return false;
  };

  conn.onclose = (e) => {
    console.log("Socket closed. Reconnect in 1 second. ", e.reason);
  };

  conn.onerror = (e) => {
    console.error("Socket encountered an error: ", e.message, "Closing socket");
    conn.close();
  };
}

connect();
