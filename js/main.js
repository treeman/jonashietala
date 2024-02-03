console.log(window.location.pathname);
console.log(window.location.href);

conn = new WebSocket("ws://127.0.0.1:8081");

conn.onmessage = (event) => {
  console.log(event);
  const msg = JSON.parse(event.data);
  console.log(msg);

  switch (msg.type) {
    case "RefreshAll":
      console.log("refresh all");
      location.reload(true);
    case "RefreshPage":
      if (msg.path == window.location.pathname) {
        console.log("refresh page");
        location.reload(true);
      }
  }

  return false;
};
