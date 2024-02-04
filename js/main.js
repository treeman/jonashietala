function connect() {
  conn = new WebSocket("ws://127.0.0.1:8081");

  conn.onopen = () => {
    console.log("Connected to ws");
  };

  conn.onmessage = (event) => {
    const msg = JSON.parse(event.data);

    switch (msg.type) {
      case "RefreshAll":
        console.log("refresh all");
        location.reload();
        break;
      case "RefreshPage":
        if (msg.path == window.location.pathname) {
          console.log("refresh page");
          location.reload();
        } else {
          console.log(
            "skip path: ",
            msg.path,
            " this path: ",
            window.location.pathname
          );
        }
        break;
      case "PositionPage":
        const cursor = msg.linenum / msg.linecount;
        // console.log("cursor:", cursor);
        // console.log("scrollMaxY:", window.scrollMaxY);
        // console.log("screen height:", window.screen.height);
        const target = window.scrollMaxY * cursor - window.screen.height / 6;
        // console.log("Scrolling...", target);
        window.scrollTo(0, target);
        break;
      default:
        console.log("Unknown message: ", msg);
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
