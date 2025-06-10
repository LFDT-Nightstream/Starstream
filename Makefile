.PHONY: all

.PHONY: cargo
all: cargo
cargo:
	@cargo build --all-targets

.PHONY: vsc
all: vsc
vsc:
	@cd starstream_vscode && npm i && npx vsce package

.PHONY: website
all: website
website:
	@cd website && npm i && npm run build
