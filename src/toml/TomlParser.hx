package toml;

using Lambda;
using StringTools;

@:using(toml.TomlParser.ValueTypeTools)
private enum ValueType {
	VInt(v:Int);
	VFloat(f:Float);
	VString(s:String);
	VBool(b:Bool);
	VArray(a:Array<ValueType>);
	VMap(m:Map<String, ValueType>);
	VDate(d:Date);
	VComment(s:String);
	VNull;
}

private enum TokenType {
	TKey(s:String);
	TString(s:String);
	TInteger(i:Int);
	TFloat(f:Float);
	TBool(b:Bool);
	TDate(d:Date);
	TNull;
	TEquals;
	TComma;
	TLeftBracket;
	TRightBracket;
	TLeftBrace;
	TRightBrace;
	TLeftDoubleBracket;
	TRightDoubleBracket;
	TNewline;
	TComment(s:String);
	TEof;
}

private class Token {
	public final type:TokenType;
	public final position:Int;
	public final length:Int;

	public function new(type:TokenType, position:Int, length:Int) {
		this.type = type;
		this.position = position;
		this.length = length;
	}
}

private class Lexer {
	final input:String;
	final inputLength:Int;
	var pos = 0;

	public function new(input:String) {
		this.input = input;
		this.inputLength = input.length;
	}

	public function tokenize():Array<Token> {
		final tokens:Array<Token> = [];
		pos = 0;

		while (pos < inputLength) {
			skipWhitespace();
			if (pos >= inputLength) break;

			final token = nextToken();
			if (token != null) tokens.push(token);
		}

		tokens.push(new Token(TEof, pos, 0));
		return tokens;
	}

	function skipWhitespace():Void {
		while (pos < inputLength && isWhitespace(input.fastCodeAt(pos))) {
			pos++;
		}
	}

	function isWhitespace(char:Int):Bool {
		return char == " ".code || char == "\t".code;
	}

	function isNewline(char:Int):Bool {
		return char == "\n".code || char == "\r".code;
	}

	function isAlpha(char:Int):Bool {
		return (char >= "A".code && char <= "Z".code)
			|| (char >= "a".code && char <= "z".code);
	}

	function isDigit(char:Int):Bool {
		return char >= "0".code && char <= "9".code;
	}

	function isAlphaNumeric(char:Int):Bool {
		return isAlpha(char) || isDigit(char) || char == "_".code || char == "-".code;
	}

	function nextToken():Token {
		if (pos >= inputLength) return null;

		final char = input.fastCodeAt(pos);
		final startPos = pos;

		// Handle newlines
		if (isNewline(char)) {
			if (char == "\r".code && pos + 1 < inputLength
				&& input.fastCodeAt(pos + 1) == "\n".code) {
				pos += 2; // \r\n
			} else {
				pos++; // \n or \r
			}
			return new Token(TNewline, startPos, pos - startPos);
		}

		// Handle comments
		if (char == "#".code) {
			return parseComment(startPos);
		}

		// Handle strings
		if (char == "\"".code) {
			return parseString(startPos, "\"".code);
		}

		if (char == "'".code) {
			return parseString(startPos, "'".code);
		}

		// Handle multiline strings
		if (char == "\"".code
			&& pos + 2 < inputLength
			&& input.fastCodeAt(pos + 1) == "\"".code
			&& input.fastCodeAt(pos + 2) == "\"".code) {
			return parseMultilineString(startPos, "\"".code);
		}

		if (char == "'".code
			&& pos + 2 < inputLength
			&& input.fastCodeAt(pos + 1) == "'".code
			&& input.fastCodeAt(pos + 2) == "'".code) {
			return parseMultilineString(startPos, "'".code);
		}

		// Handle special characters
		switch char {
			case "=".code:
				pos++;
				return new Token(TEquals, startPos, 1);
			case ",".code:
				pos++;
				return new Token(TComma, startPos, 1);
			case "[".code:
				if (pos + 1 < inputLength && input.fastCodeAt(pos + 1) == "[".code) {
					pos += 2;
					return new Token(TLeftDoubleBracket, startPos, 2);
				}
				pos++;
				return new Token(TLeftBracket, startPos, 1);
			case "]".code:
				if (pos + 1 < inputLength && input.fastCodeAt(pos + 1) == "]".code) {
					pos += 2;
					return new Token(TRightDoubleBracket, startPos, 2);
				}
				pos++;
				return new Token(TRightBracket, startPos, 1);
			case "{".code:
				pos++;
				return new Token(TLeftBrace, startPos, 1);
			case "}".code:
				pos++;
				return new Token(TRightBrace, startPos, 1);
		}

		// Handle numbers and identifiers
		if (isDigit(char) || char == "-".code || char == "+".code) {
			return parseNumber(startPos);
		}

		if (isAlpha(char)) {
			return parseIdentifier(startPos);
		}

		// Unknown character, skip it
		pos++;
		return null;
	}

	function parseComment(startPos:Int):Token {
		pos++; // skip #
		final start = pos;
		while (pos < inputLength && !isNewline(input.fastCodeAt(pos))) {
			pos++;
		}
		final comment = input.substring(start, pos);
		return new Token(TComment(comment), startPos, pos - startPos);
	}

	function parseString(startPos:Int, quote:Int):Token {
		pos++; // skip opening quote
		final start = pos;
		final value = new StringBuf();

		while (pos < inputLength) {
			final char = input.fastCodeAt(pos);

			if (char == quote) {
				pos++; // skip closing quote
				return new Token(TString(value.toString()), startPos, pos - startPos);
			}

			if (char == "\\".code) {
				pos++;
				if (pos < inputLength) {
					final escaped = input.fastCodeAt(pos);
					switch escaped {
						case "n".code:
							value.addChar("\n".code);
						case "r".code:
							value.addChar("\r".code);
						case "t".code:
							value.addChar("\t".code);
						case "\\".code:
							value.addChar("\\".code);
						case "\"".code:
							value.addChar("\"".code);
						case "'".code:
							value.addChar("'".code);
						default:
							value.addChar("\\".code);
							value.addChar(escaped);
					}
					pos++;
				}
			} else {
				value.addChar(char);
				pos++;
			}
		}

		// Unterminated string
		return new Token(TString(value.toString()), startPos, pos - startPos);
	}

	function parseMultilineString(startPos:Int, quote:Int):Token {
		pos += 3; // skip opening quotes
		final value = new StringBuf();

		// Skip first newline if present
		if (pos < inputLength && isNewline(input.fastCodeAt(pos))) {
			if (input.fastCodeAt(pos) == "\r".code && pos + 1 < inputLength
				&& input.fastCodeAt(pos + 1) == "\n".code) {
				pos += 2;
			} else {
				pos++;
			}
		}

		while (pos + 2 < inputLength) {
			if (input.fastCodeAt(pos) == quote && input.fastCodeAt(pos + 1) == quote
				&& input.fastCodeAt(pos + 2) == quote) {
				pos += 3;
				return new Token(TString(value.toString()), startPos, pos - startPos);
			}

			final char = input.fastCodeAt(pos);
			if (char == "\\".code && quote == "\"".code) { // backslash in double quotes only
				pos++;
				if (pos < inputLength) {
					final escaped = input.fastCodeAt(pos);
					switch escaped {
						case "n".code:
							value.addChar("\n".code);
						case "r".code:
							value.addChar("\r".code);
						case "t".code:
							value.addChar("\t".code);
						case "\\".code:
							value.addChar("\\".code);
						case "\"".code:
							value.addChar("\"".code);
						default:
							value.addChar("\\".code);
							value.addChar(escaped);
					}
					pos++;
				}
			} else {
				value.addChar(char);
				pos++;
			}
		}

		return new Token(TString(value.toString()), startPos, pos - startPos);
	}

	function parseNumber(startPos:Int):Token {
		var hasSign = false;
		var char = input.fastCodeAt(pos);

		if (char == "-".code || char == "+".code) {
			hasSign = true;
			pos++;
		}

		// Handle special prefixes
		if (pos + 1 < inputLength && input.fastCodeAt(pos) == "0".code) {
			final next = input.fastCodeAt(pos + 1);
			if (next == "x".code) { // 0x
				return parseHexNumber(startPos);
			} else if (next == "o".code) { // 0o
				return parseOctalNumber(startPos);
			} else if (next == "b".code) { // 0b
				return parseBinaryNumber(startPos);
			}
		}

		var hasDecimal = false;
		var hasUnderscore = false;

		while (pos < inputLength) {
			char = input.fastCodeAt(pos);

			if (isDigit(char)) {
				pos++;
			} else if (char == "_".code) {
				hasUnderscore = true;
				pos++;
			} else if (char == ".".code && !hasDecimal) {
				hasDecimal = true;
				pos++;
			} else {
				break;
			}
		}

		var numStr = input.substring(startPos, pos);
		if (hasUnderscore) numStr = numStr.replace("_", "");

		// Check for special float values
		final lowerStr = numStr.toLowerCase();
		switch lowerStr {
			case "nan", "+nan":
				return new Token(TFloat(Math.NaN), startPos, pos - startPos);
			case "-nan":
				return new Token(TFloat(-Math.NaN), startPos, pos - startPos);
			case "inf", "+inf":
				return new Token(TFloat(Math.POSITIVE_INFINITY), startPos, pos - startPos);
			case "-inf":
				return new Token(TFloat(Math.NEGATIVE_INFINITY), startPos, pos - startPos);
		}

		if (hasDecimal) {
			final f = Std.parseFloat(numStr);
			return new Token(TFloat(f), startPos, pos - startPos);
		} else {
			final i = Std.parseInt(numStr);
			return new Token(TInteger(i), startPos, pos - startPos);
		}
	}

	function parseHexNumber(startPos:Int):Token {
		pos += 2; // skip 0x
		while (pos < inputLength) {
			final char = input.fastCodeAt(pos);
			if (isDigit(char)
				|| (char >= "A".code && char <= "F".code)
				|| (char >= "a".code && char <= "f".code)
				|| char == "_".code) {
				pos++;
			} else {
				break;
			}
		}
		final numStr = input.substring(startPos, pos).replace("_", "");
		final i = Std.parseInt(numStr);
		return new Token(TInteger(i), startPos, pos - startPos);
	}

	function parseOctalNumber(startPos:Int):Token {
		pos += 2; // skip 0o
		while (pos < inputLength) {
			final char = input.fastCodeAt(pos);
			if (char >= "0".code && char <= "7".code || char == "_".code) {
				pos++;
			} else {
				break;
			}
		}
		final numStr = input.substring(startPos + 2, pos).replace("_", "");
		final i = convertOctalString(numStr);
		return new Token(TInteger(i), startPos, pos - startPos);
	}

	function parseBinaryNumber(startPos:Int):Token {
		pos += 2; // skip 0b
		while (pos < inputLength) {
			final char = input.fastCodeAt(pos);
			if (char == "0".code || char == "1".code || char == "_".code) {
				pos++;
			} else {
				break;
			}
		}
		final numStr = input.substring(startPos + 2, pos).replace("_", "");
		final i = convertBinaryString(numStr);
		return new Token(TInteger(i), startPos, pos - startPos);
	}

	function parseIdentifier(startPos:Int):Token {
		while (pos < inputLength && isAlphaNumeric(input.fastCodeAt(pos))) {
			pos++;
		}

		final value = input.substring(startPos, pos);

		// Check for date format
		final dateRegex = ~/^\d{4}-\d{2}-\d{2}$/;
		if (dateRegex.match(value)) {
			return new Token(TDate(Date.fromString(value)), startPos, pos - startPos);
		}

		// Check for boolean values
		switch value {
			case "true":
				return new Token(TBool(true), startPos, pos - startPos);
			case "false":
				return new Token(TBool(false), startPos, pos - startPos);
			case "null":
				return new Token(TNull, startPos, pos - startPos);
			default:
				return new Token(TKey(value), startPos, pos - startPos);
		}
	}

	static function convertBinaryString(s:String):Int {
		var result = 0;
		for (i in 0...s.length) {
			if (s.charAt(i) == "1") {
				result = (result << 1) | 1;
			} else {
				result = result << 1;
			}
		}
		return result;
	}

	static function convertOctalString(s:String):Int {
		var result = 0;
		for (i in 0...s.length) {
			final digit = Std.parseInt(s.charAt(i));
			result = (result << 3) | digit;
		}
		return result;
	}
}

class TomlParser {
	var tokens:Array<Token>;
	var pos = 0;

	public function new() {}

	public static function parse(text:String):TOMLSection {
		return new TomlParser().parseFromString(text);
	}

	public function parseFromString(text:String):TOMLSection {
		final lexer = new Lexer(text);
		tokens = lexer.tokenize();
		pos = 0;

		final root = new TOMLSection("root");
		var currentSection = root;

		while (!isAtEnd()) {
			skipNewlines();
			if (isAtEnd()) break;

			final token = peek();
			switch token.type {
				case TLeftDoubleBracket:
					currentSection = parseTableArray(root);
				case TLeftBracket:
					currentSection = parseTable(root);
				case TKey(_):
					parseKeyValue(currentSection);
				case TComment(s):
					parseComment(currentSection);
				default:
					advance(); // skip unknown tokens
			}

			skipNewlines();
		}

		return root;
	}

	function parseTable(root:TOMLSection):TOMLSection {
		advance(); // consume [
		final nameToken = advance();
		final name = switch nameToken.type {
			case TKey(s): s;
			case TString(s): s;
			default: "";
		}
		advance(); // consume ]
		return root.getSectionByName(name);
	}

	function parseTableArray(root:TOMLSection):TOMLSection {
		advance(); // consume [[
		final nameToken = advance();
		final name = switch nameToken.type {
			case TKey(s): s;
			case TString(s): s;
			default: "";
		}
		advance(); // consume ]]
		return root.getSectionByName(name, true, true);
	}

	function parseKeyValue(section:TOMLSection):Void {
		final keyToken = advance();
		final key = switch keyToken.type {
			case TKey(s): s;
			case TString(s): s;
			default: "";
		}

		advance(); // consume =
		final value = parseValue();
		section.addKey(key, value);
	}

	function parseComment(section:TOMLSection):Void {
		final commentToken = advance();
		final comment = switch commentToken.type {
			case TComment(s): s;
			default: "";
		}
		section.addComment(comment);
	}

	function parseValue():ValueType {
		final token = advance();
		return switch token.type {
			case TString(s): VString(s);
			case TInteger(i): VInt(i);
			case TFloat(f): VFloat(f);
			case TBool(b): VBool(b);
			case TDate(d): VDate(d);
			case TNull: VNull;
			case TComment(s): VComment(s);
			case TLeftBracket: parseArray();
			case TLeftBrace: parseInlineTable();
			default: VNull;
		}
	}

	function parseArray():ValueType {
		final values:Array<ValueType> = [];

		while (!check(TRightBracket) && !isAtEnd()) {
			skipNewlines();
			if (check(TRightBracket)) break;

			values.push(parseValue());

			skipNewlines();
			if (check(TComma)) {
				advance();
				skipNewlines();
			}
		}

		advance(); // consume ]
		return VArray(values);
	}

	function parseInlineTable():ValueType {
		final map:Map<String, ValueType> = new Map();

		while (!check(TRightBrace) && !isAtEnd()) {
			final keyToken = advance();
			final key = switch keyToken.type {
				case TKey(s): s;
				case TString(s): s;
				default: "";
			}

			advance(); // consume =
			final value = parseValue();
			map.set(key, value);

			if (check(TComma)) {
				advance();
			} else {
				break;
			}
		}

		advance(); // consume }
		return VMap(map);
	}

	function peek():Token {
		if (pos >= tokens.length) return tokens[tokens.length - 1];
		return tokens[pos];
	}

	function advance():Token {
		if (pos < tokens.length - 1) pos++;
		return tokens[pos - 1];
	}

	function check(type:TokenType):Bool {
		final token = peek();
		return Type.enumEq(token.type, type);
	}

	function isAtEnd():Bool {
		return check(TEof);
	}

	function skipNewlines():Void {
		while (check(TNewline) || check(TComment(""))) {
			advance();
		}
	}
}

class TOMLSection {
	public final subSections:Array<TOMLSection> = [];
	public final keys:Map<String, ValueType> = [];
	public final comments:Array<String> = [];
	public var name = "";
	public var isTable = false;

	public function new(name:String) {
		this.name = name;
	}

	public function addKey(key:String, value:ValueType):Void {
		final dotPos = key.indexOf(".");
		if (dotPos != -1) {
			final prefix = key.substring(0, dotPos);
			final suffix = key.substring(dotPos + 1);
			final section = getSectionByName(prefix);
			section.addKey(suffix, value);
		} else {
			keys.set(key, value);
		}
	}

	public function addComment(comment:String):Void {
		comments.push(comment);
	}

	public function getSectionByName(name:String, doCreate:Bool = true, createAsTable:Bool = false):TOMLSection {
		final dotPos = name.indexOf(".");
		if (dotPos != -1) {
			final prefix = name.substring(0, dotPos);
			final suffix = name.substring(dotPos + 1);
			return getSectionByName(prefix, doCreate, false)
				.getSectionByName(suffix, doCreate, createAsTable);
		}

		for (section in subSections) {
			if (section.name == name) {
				if (createAsTable) {
					final newSection = new TOMLSection(name);
					section.subSections.push(newSection);
					section.isTable = true;
					return newSection;
				}
				return section;
			}
		}

		if (doCreate) {
			final section = new TOMLSection(name);
			subSections.push(section);

			if (createAsTable) {
				final tableSection = new TOMLSection(name);
				section.subSections.push(tableSection);
				section.isTable = true;
				return tableSection;
			}

			return section;
		}

		return null;
	}

	public function getValue():Dynamic {
		if (isTable) {
			return subSections.map(section -> section.getValue());
		}

		final result:Map<String, Dynamic> = new Map();

		for (key => value in keys) {
			result.set(key, value.valueToDynamic());
		}

		for (section in subSections) {
			result.set(section.name, section.getValue());
		}

		return result;
	}

	public function prettyPrint():String {
		final output = new StringBuf();

		if (name != "" && name != "root") {
			output.add("[" + name + "]\n");
		}

		// Print comments first
		for (comment in comments) {
			output.add("#" + comment + "\n");
		}

		// Print key-value pairs
		for (key => value in keys) {
			switch value {
				case VComment(s):
					output.add("#" + s + "\n");
				default:
					output.add(key + " = " + valueToString(value) + "\n");
			}
		}

		// Print sub-sections
		for (section in subSections) {
			if (section.isTable) {
				for (tableEntry in section.subSections) {
					output.add("\n[[" + section.name + "]]\n");
					output.add(tableEntry.prettyPrint());
				}
			} else {
				output.add("\n" + section.prettyPrint());
			}
		}

		return output.toString();
	}

	function valueToString(value:ValueType):String {
		return switch value {
			case VInt(i): Std.string(i);
			case VFloat(f): Std.string(f);
			case VString(s): '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"';
			case VBool(b): b ? "true" : "false";
			case VDate(d): DateTools.format(d, "%Y-%m-%d");
			case VArray(a): "[" + a.map(valueToString).join(", ") + "]";
			case VMap(m):
				final pairs = [];
				for (k => v in m) {
					pairs.push(k + " = " + valueToString(v));
				}
				"{ " + pairs.join(", ") + " }";
			case VComment(s): "#" + s;
			case VNull: "null";
		}
	}
}

private class ValueTypeTools {
	public static function valueToDynamic(value:ValueType):Dynamic {
		return switch value {
			case VInt(i): i;
			case VFloat(f): f;
			case VString(s): s;
			case VBool(b): b;
			case VDate(d): d;
			case VArray(a): a.map(valueToDynamic);
			case VMap(m):
				final result:Map<String, Dynamic> = new Map();
				for (k => v in m) {
					result.set(k, valueToDynamic(v));
				}
				result;
			case VComment(s): s; // Return comment text for printing
			case VNull, null: null;
		}
	}
}
