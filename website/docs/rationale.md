# Architecture

> _This page explains the motivation and reasoning behind creating Starstream as a new virtual machine, and why existing solutions were not sufficient._

Established blockchain Virtual Machines (VMs) like have set robust foundations and created vibrant ecosystems. Given their demonstrated success, introducing another VM like Starstream requires a thoughtful rationale.

Starstream aims to address specific, important trade-offs and limitations in existing VMs by implementing a novel architecture ("coroutines") that not only simplifies development, but also makes Starstream able to fully leverage the power of zero-knowledge (ZK) technologies compared to VMs that previously designed. Notably, Starstream has the following key goals:
1. **Optionality & Upgradability**: Starstream, instead of reinventing the wheel from scratch, instead helps convert any blockchain-agnostic zkVM into a UTXO-based zkVM. This makes it easier to keep the Starstream developer experience while swapping out different proof systems (lattice vs elliptic curve) or even different target architectures (wasm vs risc-v)  
1. **Developer experience**: Starstream is not just a VM, but also comes with a Starstream DSL to not only help developers maximally take advantage of its strengths, but also to provide a great developer experience for UTXO smart contracts.
1. **Client-first**: Starstream proof generation is optimized to run on user devices and in the browser without requiring proof outsourcing to a centralized server. This not only helps ensure decentralization, but also enables a use-cases where proof outsource if often not viable (ex: games)
1. **Confidential computation**: Starstream allows generating proofs of data without revealing the underlying data. This is important for many use-case such as RWA ("Real World Assets") where, for regulatory purposes, underlying data cannot be shared, or for use-cases like gaming where strategy depends on fog-of-war.

Starstream, unlike many solutions, leverages ZK for data protection (private computation) AND scalability (folding schemes)
<details>
  <summary>Basics: what is ZK, and how does Starstream use it differently from other solutions?</summary>

ZK projects often tackle one of these two problems to varying degrees:

1. **Scalability**: In traditional blockchains, when you call a smart contract, all the nodes in the network will repeat the computation and check they got the same result (slow. This is why you have to pay "gas fees"). However, it doesn't have to be that way. There are many problems where finding an answer is much harder than checking an answer (think: puzzles are often hard to solve, but easy to check if somebody got the answer right). Notably, ZK is often used as a technique to increase the amount of computation the user has to do on their machine in order to decrease the amount of computation the network as a whole has to do (think: solving the puzzle locally, then sharing the answer for everybody to quickly check).
2. **Data protection**: In traditional blockchains, when you call a smart contract, you are sharing all the information about what you want to do (ex: I want to buy token X at price Y). This public information is problematic, as in DeFi it can lead to front-running, for RWAs it can lead to data compliance issues, and for games it makes it impossible to have fog of war. ZK can solve this by sharing the result of the computation without sharing how you did the computation in the first place (think: share the solution to a puzzle without sharing how you solved the puzzle)

Starstream, unlike a lot of ZK solutions, tackles both:
1. **Scalability**: Unlike many ZK solutions which have limited scalability benefits, Starstream allows batching together an infinite amount of computation together in a single proof (sometimes called "recursion" or "folding") allowing significantly more complex applications to be built with it.
2. **Data protection**: Unlike many ZK solutions which don't tackle data protection at all, Starstream supports computation over private data. This means, for example, you can prove you do not live in the US without having to reveal your identity or address, or prove you own an NFT in a collection without having to reveal your address.
</details>

Starstream, unlike other solutions, uses the UTXO model (like Bitcoin) instead of the account model for better proof parallelization, faster proofs and lower system requirements.
<details>
  <summary>Basics: what is the UTXO model? How does it differ from the account model?</summary>

  In the UTXO model (used by Bitcoin and Cardano), tokens behave more like physical coins. Each coin is an indivisible chunk of value that can only be spent once (which is compensated by having change given back to you if you overpaid). Each coin is independent of the rest of the system (in the same way physical coins sit in your pocket) and can only be spent if the right conditions are met (i.e. you, as the coin owner, takes them out of your pocket). Unlike the account model that is global in nature (you don't own ERC20 tokens - they're owned by an shared ERC20 contract, and the contract is the one that specifies who can control how much of the balance), the UTXO model is more naturally sharded as computation is more local which helps in terms of parallelism.

  By contrast, the account model (used by Ethereum) has transactions that mutate this global state directly. This works well in a non-ZK setting, but often leads to concurrency issues where two proofs conflict in a ZK setting (which many protocols solve by having a single "prover" that executes transactions in order).
</details>

Starstream, unlike other zkVMs, defines no opcodes. Instead, it allows turning an arbitrary underlying zkVM into a UTXO-based zkVM ready for use in decentralized systems
<details>
  <summary>Basics: what is zkVMs, and how does Starstream differ from them?</summary>

  TODO
</details>

Short explanation: Starstream, instead of reinventing the wheel by creating a whole new way of generating proofs of general computer programs like WASM (or other instruction sets like risc-v), instead built a system that takes an existing general prover and turns it into a system that can prove the correctness of a UTXO virtual machine while using the underlying zkVM to proof system to generate proof of individual scripts.

There are multiple benefits to this:
Upgradability: being able to swap out the underlying zkVM as new technology comes out with minimal effort, Starstream can stay competitive as the industry evolves both in performance and in functionality (ex: upgrading for quantum resistance)
Time-to-market: leveraging existing industry efforts to build zkVMs, we can ship Starstream to market much faster than reinventing the wheel ourselves
Cost: cooperating with existing zkVM teams, we can spread out engineering costs, leading to a much lower cost to the Cardano treasury
Parallelism: this model allows us to isolate each UTXO as a separate proof instance, allowing us to gain all the benefits of the UTXO model.

More concretely, we picked Nova as the initial zkVM given we believe it aligns best with the Cardano community’s desire for decentralization: Nova is able to run locally on consumer grade hardware in the browser (and even support private computation as well).

You can find a more technical deep-dive into technology behind Starstream in the “supporting links” section.

Roadmap:
- quantum resistant
- risc-v