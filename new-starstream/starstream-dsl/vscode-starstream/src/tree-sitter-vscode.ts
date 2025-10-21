import * as fs from 'fs';
import path from 'path';
import * as vscode from 'vscode';
import * as ts from 'web-tree-sitter';
import { Parser } from 'web-tree-sitter';

const OUTPUT_CHANNEL = vscode.window.createOutputChannel("tree-sitter-vscode");

// VSCode default token types and modifiers from:
// https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#standard-token-types-and-modifiers
const TOKEN_TYPES = [
	'namespace', 'class', 'enum', 'interface', 'struct', 'typeParameter', 'type', 'parameter', 'variable', 'property',
	'enumMember', 'decorator', 'event', 'function', 'method', 'macro', 'label', 'comment', 'string', 'keyword',
	'number', 'regexp', 'operator',
];
const TOKEN_MODIFIERS = [
	'declaration', 'definition', 'readonly', 'static', 'deprecated', 'abstract', 'async', 'modification',
	'documentation', 'defaultLibrary',
];
const LEGEND = new vscode.SemanticTokensLegend(TOKEN_TYPES, TOKEN_MODIFIERS);

type SemanticTokenTypeMapping = { targetTokenType: string, targetTokenModifiers?: string[] };
type Config = {
	lang: string,
	parser: string,
	highlights: string,
	injections?: string,
	injectionOnly: boolean,
	semanticTokenTypeMappings?: { [sourceSemanticTokenType: string]: SemanticTokenTypeMapping },
};
type Language = {
	parser: Parser,
	highlightQuery: ts.Query,
	injectionQuery?: ts.Query,
	semanticTokenTypeMappings?: { [sourceSemanticTokenType: string]: SemanticTokenTypeMapping },
};
type Token = {
	range: vscode.Range,
	type: string,
	modifiers: string[],
};
type Injection = {
	range: vscode.Range,
	tokens: Token[],
};

function log(messageOrCallback: string | (() => string), data?: any) {
	// Only log in debug mode
	const config = vscode.workspace.getConfiguration("tree-sitter-vscode");
	const isDebugMode = config.get("debug", false);

	if (isDebugMode) {
		const timestamp = new Date().toISOString();
		const message = typeof messageOrCallback === "function" ? messageOrCallback() : messageOrCallback;
		OUTPUT_CHANNEL.appendLine(`[${timestamp}] ${message}`);
		if (data) {
			OUTPUT_CHANNEL.appendLine(JSON.stringify(data, null, 2));
		}
	}
}

/**
 * Called once on extension initialization and again if the reload command is triggered.
 * It reads the configuration and registers the semantic tokens provider.
 */
export function activate(context: vscode.ExtensionContext) {
	log("Extension activated");
	// setup the semantic tokens provider
	const rawConfigs = vscode.workspace.getConfiguration("tree-sitter-vscode").get("languageConfigs");
	const configs = parseConfigs(rawConfigs);
	log(() => { return `Configured languages: ${configs.map((c) => c.lang).join(", ")}`; });
	const languageMap = configs
		.filter(config => !config.injectionOnly)
		.map(config => { return { language: config.lang }; });
	const provider = vscode.languages.registerDocumentSemanticTokensProvider(
		languageMap,
		new SemanticTokensProvider(configs),
		LEGEND,
	);
	context.subscriptions.push(provider);

	// setup the reload command
	const reload = vscode.commands.registerCommand("tree-sitter-vscode.reload",
		() => {
			// dispose of the old providers and clear the list of subscriptions
			reload.dispose();
			provider.dispose();
			context.subscriptions.length = 0;
			// reinitialize the extension
			activate(context);
		});
	context.subscriptions.push(reload);
}

/**
 * Called when the extension is deactivated.
 */
export function deactivate() { }

function parseConfigs(configs: any): Config[] {
	if (!Array.isArray(configs)) {
		throw new TypeError("Expected a list.");
	}
	return configs.map(config => {
		const lang = config["lang"];
		const parser = config["parser"];
		const highlights = config["highlights"];
		const injections = config["injections"];
		let injectionOnly = config["injectionOnly"];
		const semanticTokenTypeMappings = config["semanticTokenTypeMappings"];
		if (typeof lang !== "string") {
			throw new TypeError("Expected `lang` to be a string.");
		}
		if (typeof parser !== "string") {
			throw new TypeError("Expected `parser` to be a string.");
		}
		if (typeof highlights !== "string") {
			throw new TypeError("Expected `highlights` to be a string.");
		}
		if (injections !== undefined && typeof injections !== "string") {
			throw new TypeError("Expected `injections` to be a string.");
		}
		if (injectionOnly !== undefined && typeof injectionOnly !== "boolean") {
			throw new TypeError("Expected `injectionOnly` to be a boolean.");
		}
		if (semanticTokenTypeMappings !== undefined && (typeof semanticTokenTypeMappings !== "object" || semanticTokenTypeMappings === null)) {
			throw new TypeError("Expected `semanticTokenTypeMappings` to be an object.");
		}
		if (injectionOnly === undefined) {
			injectionOnly = false;
		}
		return { lang, parser, highlights, injections, injectionOnly, semanticTokenTypeMappings };
	}).map(config => {
		const parser = toAbsolutePath(config.parser);
		const highlights = toAbsolutePath(config.highlights);
		const injections = config.injections !== undefined ? toAbsolutePath(config.injections) : undefined;
		return { ...config, parser, highlights, injections };
	});
}

function toAbsolutePath(file: string): string {
	if (path.isAbsolute(file)) {
		return file;
	}

	const workspaceFolders = vscode.workspace.workspaceFolders;
	if (!workspaceFolders || workspaceFolders.length === 0) {
		throw new Error("Trying to resolve a relative path, but no workspace folder is open.");
	}

	const workspaceRoot = workspaceFolders[0].uri.fsPath;
	return path.resolve(workspaceRoot, file);
}


async function initLanguage(config: Config): Promise<Language> {
	log(() => { return `Initializing language: ${config.lang}`; });
	await Parser.init().catch();
	const parser = new Parser;
	const lang = await ts.Language.load(config.parser);
	log(`Tree-Sitter ABI version for ${config.lang} is ${lang.abiVersion}.`);
	parser.setLanguage(lang);
	const queryText = fs.readFileSync(config.highlights, "utf-8");
	const highlightQuery = new ts.Query(lang, queryText);
	let injectionQuery = undefined;
	if (config.injections !== undefined) {
		const injectionText = fs.readFileSync(config.injections, "utf-8");
		injectionQuery = new ts.Query(lang, injectionText);
	}
	return { parser, highlightQuery, injectionQuery, semanticTokenTypeMappings: config.semanticTokenTypeMappings };
}

function convertPosition(pos: ts.Point): vscode.Position {
	return new vscode.Position(pos.row, pos.column);
}

function addPosition(range: vscode.Range, pos: vscode.Position): vscode.Range {
	const start = (range.start.line == 0)
		? new vscode.Position(range.start.line + pos.line, range.start.character + pos.character)
		: new vscode.Position(range.start.line + pos.line, range.start.character);
	const end = (range.end.line == 0)
		? new vscode.Position(range.end.line + pos.line, range.end.character + pos.character)
		: new vscode.Position(range.end.line + pos.line, range.end.character);
	return new vscode.Range(start, end);
}

function parseCaptureName(name: string): { type: string, modifiers: string[] } {
	const parts = name.split(".");
	if (parts.length === 0) {
		throw new Error("Capture name is empty.");
	} else if (parts.length === 1) {
		return { type: parts[0], modifiers: [] };
	} else {
		return { type: parts[0], modifiers: parts.slice(1) };
	}
}

/**
 * Semantic tokens cannot span multiple lines,
 * so if the range doesn't end in the same line,
 * one token for each line is created.
 */
function splitToken(token: Token): Token[] {
	const start = token.range.start;
	const end = token.range.end;
	if (start.line != end.line) {
		// 100_0000 is chosen as the arbitrary length, since the actual line length is unknown.
		// Choosing a big number works, while `Number.MAX_VALUE` seems to confuse VSCode.
		const maxLineLength = 100_000;
		const lineDiff = end.line - start.line;
		if (lineDiff < 0) {
			throw new RangeError("Invalid token range");
		}
		let tokens: Token[] = [];
		// token for the first line, beginning at the start char
		tokens.push({
			range: new vscode.Range(start, new vscode.Position(start.line, maxLineLength)),
			type: token.type,
			modifiers: token.modifiers
		});
		// tokens for intermediate lines, spanning from 0 to maxLineLength
		for (let i = 1; i < lineDiff; i++) {
			const middleToken: Token = {
				range: new vscode.Range(
					new vscode.Position(start.line + i, 0),
					new vscode.Position(start.line + i, maxLineLength)),
				type: token.type,
				modifiers: token.modifiers,
			};
			tokens.push(middleToken);
		}
		// token for the last line, ending at the end char
		tokens.push({
			range: new vscode.Range(new vscode.Position(end.line, 0), end),
			type: token.type,
			modifiers: token.modifiers
		});
		return tokens;
	} else {
		return [token];
	}
}

class SemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
	private readonly configs: Config[];
	private tsLangs: { [lang: string]: Language } = {};

	constructor(configs: Config[]) {
		this.configs = configs;
	}

	/**
	 * Called regularly by VSCode to provide semantic tokens for the given document.
	 * It parses the document with the corresponding language parser and returns the tokens.
	 */
	async provideDocumentSemanticTokens(
		document: vscode.TextDocument,
		token: vscode.CancellationToken
	) {
		const lang = document.languageId;
		if (!(lang in this.tsLangs)) {
			const config = this.configs.find(config => config.lang === lang);
			if (config === undefined) {
				throw new Error("No config for lang provided.");
			}
			this.tsLangs[lang] = await initLanguage(config);
		}
		const tokens = await this.parseToTokens(this.tsLangs[lang], document.getText(), { row: 0, column: 0 });
		const builder = new vscode.SemanticTokensBuilder(LEGEND);
		tokens.forEach(token => builder.push(token.range, token.type, token.modifiers));
		return builder.build();
	}

	/**
	 * Parses the given text with the given language parser and returns the highlighting tokens.
	 * Calls `getInjections` for nested injections.
	 */
	async parseToTokens(lang: Language, text: string, startPosition: ts.Point): Promise<Token[]> {
		const { parser, highlightQuery, injectionQuery } = lang;
		const tree = parser.parse(text);
		if (tree === null) {
			return [];
		}
		const matches = highlightQuery.matches(tree.rootNode);
		let tokens = this.matchesToTokens(lang, matches);
		if (injectionQuery !== undefined) {
			const injections = await this.getInjections(injectionQuery, tree.rootNode);
			// merge the injection tokens with the main tokens
			for (const injection of injections) {
				if (injection.tokens.length > 0) {
					const range = injection.range;
					tokens = tokens
						// remove all tokens that are contained in an injection
						.filter(token => !range.contains(token.range))
						// split tokens that are partially contained in an injection
						.flatMap(token => {
							if (token.range.intersection(range) !== undefined) {
								let newTokens: Token[] = [];
								if (token.range.start.isBefore(range.start)) {
									const before = new vscode.Range(token.range.start, range.start);
									newTokens.push({ ...token, range: before });
								}
								if (token.range.end.isAfter(range.end)) {
									const after = new vscode.Range(range.end, token.range.end);
									newTokens.push({ ...token, range: after });
								}
								return newTokens;
							} else {
								return [token];
							}
						});
				}
			}
			tokens = tokens.concat(injections.map(injection => injection.tokens).flat());
		}
		tokens = tokens
			.map(token => {
				return { ...token, range: addPosition(token.range, convertPosition(startPosition)) };
			});
		return tokens;
	}

	matchesToTokens(lang: Language, matches: ts.QueryMatch[]): Token[] {
		const unsplitTokens: Token[] = matches
			.flatMap(match => match.captures)
			.flatMap(capture => {
				// Store the original capture name before splitting
				const originalCaptureName = capture.name;
				let { type, modifiers: modifiers } = parseCaptureName(capture.name);
				let start = convertPosition(capture.node.startPosition);
				let end = convertPosition(capture.node.endPosition);

				// First check if we have a mapping for the original unsplit name
				if (lang.semanticTokenTypeMappings && Object.prototype.hasOwnProperty.call(lang.semanticTokenTypeMappings, originalCaptureName)) {
					const mapping = lang.semanticTokenTypeMappings[originalCaptureName];

					type = mapping.targetTokenType;
					modifiers = mapping.targetTokenModifiers ?? [];

					log(() => {
						return `Applied type mapping for original name: ${originalCaptureName} → ${mapping.targetTokenType}${mapping.targetTokenModifiers && mapping.targetTokenModifiers.length > 0
							? ` with modifiers: ${mapping.targetTokenModifiers.join(", ")}` : ""}`;
					});
				}
				// If no mapping for the full name, check for just the type
				else if (lang.semanticTokenTypeMappings && Object.prototype.hasOwnProperty.call(lang.semanticTokenTypeMappings, type)) {
					const mapping = lang.semanticTokenTypeMappings[type];

					type = mapping.targetTokenType;
					modifiers = mapping.targetTokenModifiers ?? [];

					log(() => {
						return `Applied type mapping for base type: ${type} → ${mapping.targetTokenType}${mapping.targetTokenModifiers && mapping.targetTokenModifiers.length > 0
							? ` with modifiers: ${mapping.targetTokenModifiers.join(", ")}`
							: ""}`;
					});
				}

				if (TOKEN_TYPES.includes(type)) {
					const validModifiers = modifiers.filter(modifier => TOKEN_MODIFIERS.includes(modifier));
					const token: Token = {
						range: new vscode.Range(start, end),
						type: type,
						modifiers: validModifiers
					};
					return token;
				} else {
					return [];
				}
			});

		return unsplitTokens.flatMap(token => {
			// Get all tokens contained within this token
			const contained = unsplitTokens.filter(t =>
				(!(token.range.isEqual(t.range))) && token.range.contains(t.range)
			);

			if (contained.length > 0) {
				// Sort contained tokens by their start position
				const sortedContained = contained.sort((a, b) =>
					a.range.start.compareTo(b.range.start)
				);

				let resultTokens = [];
				let currentPos = token.range.start;

				// Create tokens for the gaps between contained tokens
				for (const containedToken of sortedContained) {
					// If there's a gap before this contained token, create a token for it
					if (currentPos.compareTo(containedToken.range.start) < 0) {
						resultTokens.push({
							...token,
							range: new vscode.Range(currentPos, containedToken.range.start),
						});
					}
					currentPos = containedToken.range.end;
				}

				// Add token for the gap after the last contained token if needed
				if (currentPos.compareTo(token.range.end) < 0) {
					resultTokens.push({
						...token,
						range: new vscode.Range(currentPos, token.range.end),
					});
				}

				return resultTokens;
			} else {
				return token;
			}
		}).flatMap(splitToken);
	}

	/**
	 * Get the injection range and tokens for a specific match.
	 */
	async getInjection(match: ts.QueryMatch): Promise<Injection | null> {
		// determine language
		const {
			"injection.language": injectionLanguage,
			// TODO: add support for self and parent injections
			// "injection.self": injectionSelf,
			// "injection.parent": injectionParent
		} = (match as any).setProperties || {};
		// the language is hard coded by "set!"
		const hardCoded = typeof injectionLanguage == "string" ? injectionLanguage : undefined;
		// dynamically determined language
		const dynamic = match.captures.find(capture => capture.name === "injection.language")?.node.text;
		// custom language determination by capture name
		const name = match.captures.find(capture => this.configs.map(config => config.lang).includes(capture.name))?.name;

		const lang = hardCoded || dynamic || name;
		if (lang === undefined) return null;

		// determine capture
		let capture = undefined;
		if (hardCoded !== undefined) {
			if (match.captures.length === 0) return null;
			// use first capture (there should only be one)
			capture = match.captures[0];
		} else if (dynamic !== undefined) {
			capture = match.captures.find(capture => capture.name === "injection.content");
		} else if (name !== undefined) {
			capture = match.captures.find(capture => capture.name === name);
		}
		if (capture === undefined) return null;

		// get language config
		const config = this.configs.find(config => config.lang === lang);
		if (config === undefined) return null;

		if (!(lang in this.tsLangs)) {
			this.tsLangs[lang] = await initLanguage(config);
		}
		const langConfig = this.tsLangs[lang];

		let tokens = await this.parseToTokens(langConfig, capture.node.text, capture.node.startPosition);
		let range = new vscode.Range(convertPosition(capture.node.startPosition), convertPosition(capture.node.endPosition));
		return { range, tokens };
	}

	/**
	 * Matches the given injection query against the given node and returns the highlighting tokens.
	 * This also works for nested injections.
	 */
	async getInjections(injectionQuery: ts.Query, node: ts.Node): Promise<Injection[]> {
		const matches = injectionQuery.matches(node);
		const injections = matches.map(async match => await this.getInjection(match));
		return (await Promise.all(injections)).filter((injection): injection is Injection => injection !== null);
	}
}
