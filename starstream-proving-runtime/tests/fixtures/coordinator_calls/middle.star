contract;

import { leaf } from "./leaf.star";

script fn middle(value: i64) -> i64 {
    leaf(value) + leaf(value + 1)
}
