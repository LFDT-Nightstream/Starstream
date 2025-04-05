.PHONY: all

.PHONY: cargo
all: cargo
cargo:
	@cargo build --all-targets

.PHONY: vsc
all: vsc
vsc:
	@cd starstream_vscode && npm i && npx vsce package

target/debug/examples/self_test: cargo

target/debug/examples/self_test_permissioned: cargo
