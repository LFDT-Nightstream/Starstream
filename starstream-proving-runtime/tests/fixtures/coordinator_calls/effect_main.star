contract;

import { effect_leaf } from "./effect_leaf.star";
import { leaf } from "./leaf.star";

abi Effects {
    effect Ping();
}

script fn effect_main() {
    try {
        effect_leaf(10);
    } with Ping() {
        leaf(20);
    }
}
