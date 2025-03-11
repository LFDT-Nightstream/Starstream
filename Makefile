.PHONY: all

.PHONY: cargo
all: cargo
cargo:
	@cargo build

.PHONY: vsc
all: vsc
vsc:
	@cd starstream_vscode && npx vsce package
