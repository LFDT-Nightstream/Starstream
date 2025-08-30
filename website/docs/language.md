# Starstream DSL

The **Starstream DSL** is a domain-specific language, similar to [Compact] but [Utxo]-based instead of account based.

The star features of the Starstream DSL are:

* Defining Utxos as coroutines.
* Typed effect handling.

**Note**: the Starstream compiler and VM are still a work in progress, so not everything described below is implemented.

[Compact]: https://docs.midnight.network/develop/reference/compact/
[UTXO]: https://en.wikipedia.org/wiki/Unspent_transaction_output#The_Extended_UTXO_(EUTXO)_Model

## Program

A Starstream **program** is contained in a single source file, compiled as a unit. Each file consists of top-level items, each of which can be one of the following:

| Item                  | Syntax example             |
| --------------------- | -------------------------- |
| [Import](#import)     | To be determined           |
| [Constant](#constant) | `const Name = Expression;` |
| [Typedef](#typedef)   | `typedef Name = Type;`     |
| [Abi](#abi)           | `abi Name { ... }`         |
| [Utxo](#utxo)         | `utxo Name { ... }`        |
| [Token](#token)       | `token Name { ... }`       |
| [Script](#script)     | `script { ... }`           |

### Import

Once compiled, Starstream imports are **content-addressed**.

### Constant

Defines a reusable constant with a particular [expression](#expression) as its value.

### Typedef

Defines a reusable alias from a name to a [type](#type).

### Abi

#### Abi Function

#### Abi Effect

#### Abi Event

#### Abi Error

### Utxo

Each `utxo` block defines a new Utxo [type](#type) with a specific name. The block contains a variety of UTXO items:

| UTXO Item                        | Syntax example                 |
| -------------------------------- | ------------------------------ |
| [Main](#utxo-main)               | `main(arg: Type, ...) { ... }` |
| [Storage](#utxo-storage)         | `storage { Name: Type; ... } ` |
| [ABI implementation](#utxo-impl) | `impl AbiName { ... }`         |
<!-- TODO: Yield and Resume type declarations -->

#### Utxo Main

The `main` block contains the code that defines the primary behavior and lifetime of the Utxo. It can optionally take parameters used to construct the UTXO. The interior of the block is a series of [statements](#statement).

#### Utxo Storage

The `storage` block contains a series of public variable declarations, each with a name and a [type](#type).

#### Utxo Impl

### Token

#### Token Mint

#### Token Bind

#### Token Unbind

#### Token Burn

### Script

A **script** block contains a series of [functions](#function) which can be called by users at the top level of a Starstream transaction.

## Function

## Statement

## Expression

## Type
