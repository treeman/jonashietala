conn = new WebSocket("ws://127.0.0.1:8081");

conn.onopen = (event) => {
  conn.send("PING");
};

conn.onmessage = (event) => {
  console.log(event.data);
  return false;
};
