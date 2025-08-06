import hello from "file-loader!../../grammar/examples/hello_world.star";
import payToPublicKeyHash from "file-loader!../../grammar/examples/pay_to_public_key_hash.star";
import simpleOracle from "file-loader!../../grammar/examples/simple_oracle.star";
import oracle from "file-loader!../../grammar/examples/oracle.star";
import permissionedToken from "file-loader!../../grammar/examples/permissioned_usdc.star";
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
  "PayToPublicKeyHash": cache(fetchCode(payToPublicKeyHash)),
  "SimpleOracle": cache(fetchCode(simpleOracle)),
  "Permissioned Token": cache(fetchCode(permissionedToken)),
  Oracle: cache(fetchCode(oracle)),
} as Record<string, () => Promise<string>>;
