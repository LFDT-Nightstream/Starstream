import hello from "file-loader!../../grammar/examples/hello_world.star";
import payToPublicKeyHash from "file-loader!../../grammar/examples/pay_to_public_key_hash.star";
import simpleOracle from "file-loader!../../grammar/examples/simple_oracle.star";
import oracle from "file-loader!../../grammar/examples/oracle.star";
import permissionedToken from "file-loader!../../grammar/examples/permissioned_usdc.star";
import event from "file-loader!../../grammar/examples/event.star";
import { cache } from "react";

const fetchCode = (url: string) => async (): Promise<string> => {
  try {
    const response = await fetch(url);
    return response.text();
  } catch (error) {
    console.error("Error fetching source file:", error);
    return `/* Error: ${error} */`;
  }
};

export default {
  "Hello World": cache(fetchCode(hello)),
  Event: cache(fetchCode(event)),
  PayToPublicKeyHash: cache(fetchCode(payToPublicKeyHash)),
  "Simple Oracle": cache(fetchCode(simpleOracle)),
  Oracle: cache(fetchCode(oracle)),
  "Permissioned Token": cache(fetchCode(permissionedToken)),
} as Record<string, () => Promise<string>>;
