import init from "../build/starstream_web";

async function getBuffer(event: MessageEvent) {
  try {
    removeEventListener("message", getBuffer);
    await init(event.data);
    postMessage(`init OK`);
  } catch (e) {
    postMessage(`init error ${e}`);
  }
}

addEventListener("message", getBuffer);
