import hello from "file-loader!../../grammar/examples/hello_world.star";
import payToPublicKeyHash from "file-loader!../../grammar/examples/pay_to_public_key_hash.star";
import simpleOracle from "file-loader!../../grammar/examples/simple_oracle.star";
import effectHandlers from "file-loader!../../grammar/examples/effect_handlers.star";
import tokens from "file-loader!../../grammar/examples/tokens.star";
import oracle from "file-loader!../../grammar/examples/oracle.star";
import permissionedToken from "file-loader!../../grammar/examples/permissioned_usdc.star";
import event from "file-loader!../../grammar/examples/event.star";

const fetchCode = (url: string) => async (): Promise<string> => {
  try {
    const response = await fetch(url);
    return response.text();
  } catch (error) {
    console.error("Error fetching source file:", error);
    return `/* Error: ${error} */`;
  }
};

// Like `react.cache` but works properly on the client.
function cache<R>(fn: () => R): () => R {
  let got = false;
  let result: R;
  return () => {
    if (!got) {
      result = fn();
      got = true;
    }
    return result;
  };
}

export default {
  "Hello World": cache(fetchCode(hello)),
  Event: cache(fetchCode(event)),
  PayToPublicKeyHash: cache(fetchCode(payToPublicKeyHash)),
  "Simple Oracle": cache(fetchCode(simpleOracle)),
  "Effect Handlers": cache(fetchCode(effectHandlers)),
  "Token api": cache(fetchCode(tokens)),
  Oracle: cache(fetchCode(oracle)),
  "Permissioned Token": cache(fetchCode(permissionedToken)),
} as Record<string, () => Promise<string>>;
