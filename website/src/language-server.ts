addEventListener("message", (event: MessageEvent) => {
  console.log("Hello from worker", event.data);
});
