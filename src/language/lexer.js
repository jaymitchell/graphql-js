/* @flow /
/**
 *  Copyright (c) 2015, Facebook, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the BSD-style license found in the
 *  LICENSE file in the root directory of this source tree. An additional grant
 *  of patent rights can be found in the PATENTS file in the same directory.
 */

import type { Token } from './ast';
import type { Source } from './source';
import { syntaxError } from '../error';

/**
 * Given a Source object, this returns a Lexer for that source.
 * A Lexer is a stateful stream generator in that every time
 * it is advanced, it returns the next token in the Source. Assuming the
 * source lexes, the final Token emitted by the lexer will be of kind
 * EOF, after which the lexer will repeatedly return the same EOF token
 * whenever called.
 */
export function createLexer<TOptions>(
  source: Source,
  options: TOptions
): Lexer<TOptions> {
  const startOfFileToken = makeToken(TokenKind.SOF, 0, 0, 0, null);
  const lexer: Lexer<TOptions> = {
    source,
    options,
    lastToken: startOfFileToken,
    token: startOfFileToken,
    line: 1,
    advance: advanceLexer
  };
  return lexer;
}

function advanceLexer() {
  let token = this.lastToken = this.token;
  if (token.kind !== TokenKind.EOF) {
    do {
      token = token.next = readToken(this, token.end, token);
    } while (
      token.kind === TokenKind.COMMENT ||
      token.kind === TokenKind.COMMA
    );
    this.token = token;
  }
  return token;
}

/**
 * The return type of createLexer.
 */
export type Lexer<TOptions> = {
  source: Source;
  options: TOptions;

  /**
   * The previously focused non-ignored token.
   */
  lastToken: Token;

  /**
   * The currently focused non-ignored token.
   */
  token: Token;

  /**
   * The line containing the current token
   */
  line: number;

  /**
   * Advances the token stream to the next non-ignored token.
   */
  advance(): Token;
};

/**
 * An enum describing the different kinds of tokens that the lexer emits.
 */
export const TokenKind = {
  SOF: '<SOF>',
  EOF: '<EOF>',
  BANG: '!',
  DOLLAR: '$',
  PAREN_L: '(',
  PAREN_R: ')',
  COMMA: ',',
  SPREAD: '...',
  COLON: ':',
  EQUALS: '=',
  AT: '@',
  BRACKET_L: '[',
  BRACKET_R: ']',
  BRACE_L: '{',
  PIPE: '|',
  BRACE_R: '}',
  NAME: 'Name',
  INT: 'Int',
  FLOAT: 'Float',
  STRING: 'String',
  COMMENT: 'Comment'
};

/**
 * A helper function to describe a token as a string for debugging
 */
export function getTokenDesc(token: Token): string {
  const value = token.value;
  return value ? `${token.kind} "${value}"` : token.kind;
}

const charCodeAt = String.prototype.charCodeAt;
const slice = String.prototype.slice;

/**
 * Helper function for constructing the Token object.
 */
function makeToken(
  kind,
  start: number,
  end: number,
  line: number,
  prev: ?Token,
  value?: string
): Token {
  return { kind, start, end, line, value, prev, next: null };
}

function printCharCode(code) {
  return (
    // NaN/undefined represents access beyond the end of the file.
    isNaN(code) ? TokenKind.EOF :
    // Trust JSON for ASCII.
    code < 0x007F ? JSON.stringify(String.fromCharCode(code)) :
    // Otherwise print the escaped form.
    `"\\u${('00' + code.toString(16).toUpperCase()).slice(-4)}"`
  );
}

/**
 * Gets the next token from the source starting at the given position.
 *
 * This skips over whitespace and comments until it finds the next lexable
 * token, then lexes punctuators immediately or calls the appropriate helper
 * function for more complicated tokens.
 */
function readToken(
  lexer: Lexer<*>,
  fromPosition: number,
  prev: ?Token
): Token {
  const source = lexer.source;
  const body = source.body;
  const bodyLength = body.length;

  const position = positionAfterWhitespace(body, fromPosition, lexer);
  const line = lexer.line;

  if (position >= bodyLength) {
    return makeToken(TokenKind.EOF, bodyLength, bodyLength, line, prev);
  }

  const code = charCodeAt.call(body, position);

  // SourceCharacter
  if (code < 0x0020 && code !== 0x0009 && code !== 0x000A && code !== 0x000D) {
    throw syntaxError(
      source,
      position,
      `Invalid character ${printCharCode(code)}.`
    );
  }

  switch (code) {
    // !
    case 33:
      return makeToken(TokenKind.BANG, position, position + 1, line, prev);
    // #
    case 35:
      return readComment(source, position, line, prev);
    // $
    case 36:
      return makeToken(TokenKind.DOLLAR, position, position + 1, line, prev);
    // (
    case 40:
      return makeToken(TokenKind.PAREN_L, position, position + 1, line, prev);
    // )
    case 41:
      return makeToken(TokenKind.PAREN_R, position, position + 1, line, prev);
    // ,
    case 44:
      return makeToken(TokenKind.COMMA, position, position + 1, line, prev);
    // .
    case 46:
      if (charCodeAt.call(body, position + 1) === 46 &&
          charCodeAt.call(body, position + 2) === 46) {
        return makeToken(TokenKind.SPREAD, position, position + 3, line, prev);
      }
      break;
    // :
    case 58:
      return makeToken(TokenKind.COLON, position, position + 1, line, prev);
    // =
    case 61:
      return makeToken(TokenKind.EQUALS, position, position + 1, line, prev);
    // @
    case 64:
      return makeToken(TokenKind.AT, position, position + 1, line, prev);
    // [
    case 91:
      return makeToken(TokenKind.BRACKET_L, position, position + 1, line, prev);
    // ]
    case 93:
      return makeToken(TokenKind.BRACKET_R, position, position + 1, line, prev);
    // {
    case 123:
      return makeToken(TokenKind.BRACE_L, position, position + 1, line, prev);
    // |
    case 124:
      return makeToken(TokenKind.PIPE, position, position + 1, line, prev);
    // }
    case 125:
      return makeToken(TokenKind.BRACE_R, position, position + 1, line, prev);
    // A-Z _ a-z
    case 65: case 66: case 67: case 68: case 69: case 70: case 71: case 72:
    case 73: case 74: case 75: case 76: case 77: case 78: case 79: case 80:
    case 81: case 82: case 83: case 84: case 85: case 86: case 87: case 88:
    case 89: case 90:
    case 95:
    case 97: case 98: case 99: case 100: case 101: case 102: case 103: case 104:
    case 105: case 106: case 107: case 108: case 109: case 110: case 111:
    case 112: case 113: case 114: case 115: case 116: case 117: case 118:
    case 119: case 120: case 121: case 122:
      return readName(source, position, line, prev);
    // - 0-9
    case 45:
    case 48: case 49: case 50: case 51: case 52:
    case 53: case 54: case 55: case 56: case 57:
      return readNumber(source, position, code, line, prev);
    // "
    case 34: return readString(source, position, line, prev);
  }

  throw syntaxError(
    source,
    position,
    `Unexpected character ${printCharCode(code)}.`
  );
}

/**
 * Reads from body starting at startPosition until it finds a non-whitespace
 * or commented character, then returns the position of that character for
 * lexing.
 */
function positionAfterWhitespace(
  body: string,
  startPosition: number,
  lexer: Lexer<*>
): number {
  const bodyLength = body.length;
  let position = startPosition;
  while (position < bodyLength) {
    const code = charCodeAt.call(body, position);
    if (code === 9 || code === 32 || code === 0xFEFF) { // tab | space | BOM
      ++position;
    } else if (code === 10) { // new line
      ++position;
      ++lexer.line;
    } else if (code === 13) { // carriage return
      if (charCodeAt.call(body, position + 1) === 10) {
        position += 2;
      } else {
        ++position;
      }
      ++lexer.line;
    } else {
      break;
    }
  }
  return position;
}

/**
 * Reads a comment token from the source file.
 *
 * #[\u0009\u0020-\uFFFF]*
 */
function readComment(source, start, line, prev): Token {
  const body = source.body;
  let code;
  let position = start;

  do {
    code = charCodeAt.call(body, ++position);
  } while (
    code !== null &&
    // SourceCharacter but not LineTerminator
    (code > 0x001F || code === 0x0009)
  );

  return makeToken(
    TokenKind.COMMENT,
    start,
    position,
    line,
    prev,
    slice.call(body, start + 1, position)
  );
}

/**
 * Reads a number token from the source file, either a float
 * or an int depending on whether a decimal point appears.
 *
 * Int:   -?(0|[1-9][0-9]*)
 * Float: -?(0|[1-9][0-9]*)(\.[0-9]+)?((E|e)(+|-)?[0-9]+)?
 */
function readNumber(source, start, firstCode, line, prev): Token {
  const body = source.body;
  let code = firstCode;
  let position = start;
  let isFloat = false;

  if (code === 45) { // -
    code = charCodeAt.call(body, ++position);
  }

  if (code === 48) { // 0
    code = charCodeAt.call(body, ++position);
    if (code >= 48 && code <= 57) {
      throw syntaxError(
        source,
        position,
        `Invalid number, unexpected digit after 0: ${printCharCode(code)}.`
      );
    }
  } else {
    position = readDigits(source, position, code);
    code = charCodeAt.call(body, position);
  }

  if (code === 46) { // .
    isFloat = true;

    code = charCodeAt.call(body, ++position);
    position = readDigits(source, position, code);
    code = charCodeAt.call(body, position);
  }

  if (code === 69 || code === 101) { // E e
    isFloat = true;

    code = charCodeAt.call(body, ++position);
    if (code === 43 || code === 45) { // + -
      code = charCodeAt.call(body, ++position);
    }
    position = readDigits(source, position, code);
  }

  return makeToken(
    isFloat ? TokenKind.FLOAT : TokenKind.INT,
    start,
    position,
    line,
    prev,
    slice.call(body, start, position)
  );
}

/**
 * Returns the new position in the source after reading digits.
 */
function readDigits(source, start, firstCode) {
  const body = source.body;
  let position = start;
  let code = firstCode;
  if (code >= 48 && code <= 57) { // 0 - 9
    do {
      code = charCodeAt.call(body, ++position);
    } while (code >= 48 && code <= 57); // 0 - 9
    return position;
  }
  throw syntaxError(
    source,
    position,
    `Invalid number, expected digit but got: ${printCharCode(code)}.`
  );
}

/**
 * Reads a string token from the source file.
 *
 * "([^"\\\u000A\u000D]|(\\(u[0-9a-fA-F]{4}|["\\/bfnrt])))*"
 */
function readString(source, start, line, prev): Token {
  const body = source.body;
  let position = start + 1;
  let chunkStart = position;
  let code = 0;
  let value = '';

  while (
    position < body.length &&
    (code = charCodeAt.call(body, position)) !== null &&
    // not LineTerminator
    code !== 0x000A && code !== 0x000D &&
    // not Quote (")
    code !== 34
  ) {
    // SourceCharacter
    if (code < 0x0020 && code !== 0x0009) {
      throw syntaxError(
        source,
        position,
        `Invalid character within String: ${printCharCode(code)}.`
      );
    }

    ++position;
    if (code === 92) { // \
      value += slice.call(body, chunkStart, position - 1);
      code = charCodeAt.call(body, position);
      switch (code) {
        case 34: value += '"'; break;
        case 47: value += '\/'; break;
        case 92: value += '\\'; break;
        case 98: value += '\b'; break;
        case 102: value += '\f'; break;
        case 110: value += '\n'; break;
        case 114: value += '\r'; break;
        case 116: value += '\t'; break;
        case 117: // u
          const charCode = uniCharCode(
            charCodeAt.call(body, position + 1),
            charCodeAt.call(body, position + 2),
            charCodeAt.call(body, position + 3),
            charCodeAt.call(body, position + 4)
          );
          if (charCode < 0) {
            throw syntaxError(
              source,
              position,
              'Invalid character escape sequence: ' +
              `\\u${body.slice(position + 1, position + 5)}.`
            );
          }
          value += String.fromCharCode(charCode);
          position += 4;
          break;
        default:
          throw syntaxError(
            source,
            position,
            `Invalid character escape sequence: \\${String.fromCharCode(code)}.`
          );
      }
      ++position;
      chunkStart = position;
    }
  }

  if (code !== 34) { // quote (")
    throw syntaxError(source, position, 'Unterminated string.');
  }

  value += slice.call(body, chunkStart, position);
  return makeToken(TokenKind.STRING, start, position + 1, line, prev, value);
}

/**
 * Converts four hexidecimal chars to the integer that the
 * string represents. For example, uniCharCode('0','0','0','f')
 * will return 15, and uniCharCode('0','0','f','f') returns 255.
 *
 * Returns a negative number on error, if a char was invalid.
 *
 * This is implemented by noting that char2hex() returns -1 on error,
 * which means the result of ORing the char2hex() will also be negative.
 */
function uniCharCode(a, b, c, d) {
  return char2hex(a) << 12 | char2hex(b) << 8 | char2hex(c) << 4 | char2hex(d);
}

/**
 * Converts a hex character to its integer value.
 * '0' becomes 0, '9' becomes 9
 * 'A' becomes 10, 'F' becomes 15
 * 'a' becomes 10, 'f' becomes 15
 *
 * Returns -1 on error.
 */
function char2hex(a) {
  return (
    a >= 48 && a <= 57 ? a - 48 : // 0-9
    a >= 65 && a <= 70 ? a - 55 : // A-F
    a >= 97 && a <= 102 ? a - 87 : // a-f
    -1
  );
}

/**
 * Reads an alphanumeric + underscore name from the source.
 *
 * [_A-Za-z][_0-9A-Za-z]*
 */
function readName(source, position, line, prev): Token {
  const body = source.body;
  const bodyLength = body.length;
  let end = position + 1;
  let code = 0;
  while (
    end !== bodyLength &&
    (code = charCodeAt.call(body, end)) !== null &&
    (
      code === 95 || // _
      code >= 48 && code <= 57 || // 0-9
      code >= 65 && code <= 90 || // A-Z
      code >= 97 && code <= 122 // a-z
    )
  ) {
    ++end;
  }
  return makeToken(
    TokenKind.NAME,
    position,
    end,
    line,
    prev,
    slice.call(body, position, end)
  );
}
