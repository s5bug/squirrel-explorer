import monaco from "monaco-editor";
import IMonarchLanguage = monaco.languages.IMonarchLanguage;

const squirrelLanguage: IMonarchLanguage = {
  defaultToken: 'invalid',
  tokenPostfix: '.nut',

  keywords: [
    'break', 'case', 'catch', 'class', 'clone', 'continue',
    'const', 'default', 'delegate', 'delete', 'else', 'enum',
    'extends', 'for', 'foreach', 'function', 'if', 'in',
    'local', 'null', 'resume', 'return', 'switch', 'this',
    'throw', 'try', 'typeof', 'while', 'parent', 'yield',
    'constructor', 'vargc', 'vargv', 'instanceof', 'true', 'false',
    'static'
  ],

  operators: [
    '!', '!=', '||', '==', '&&', '<=', '=>', '>',
    '+', '+=', '-', '-=', '/', '/=', '*', '*=',
    '%', '%=', '++', '--', '<-', '=', '&', '^',
    '|', '~', '>>', '<<', '>>>'
  ],

  // we include these common regular expressions
  symbols: /[=><!~&|+\-*\/^%{}\[\]]+/,
  escapes: /\\(?:[abfnrtv\\"'0]|x[0-9A-Fa-f]{1,4})/,
  digits: /\d+/,
  octaldigits: /[0-7]+/,
  hexdigits: /[0-9a-fA-F]+/,

  // The main tokenizer for our languages
  tokenizer: {
    root: [
      [/[{}]/, 'delimiter.bracket'],
      { include: 'common' }
    ],

    common: [
      // identifiers and keywords
      [/[a-zA-Z_]+[a-zA-Z_0-9]*/, {
        cases: {
          '@keywords': 'keyword',
          '@default': 'identifier'
        }
      }],

      // whitespace
      { include: '@whitespace' },

      // delimiters and operators
      [/[()\[\]]/, '@brackets'],
      [/[<>](?!@symbols)/, '@brackets'],
      [/@symbols/, {
        cases: {
          '@operators': 'delimiter',
          '@default': ''
        }
      }],

      // numbers
      [/(@digits)\.(@digits)?([eE][\-+]?(@digits))?/, 'number.float'],
      [/0[xX](@hexdigits)/, 'number.hex'],
      [/0(@octaldigits)/, 'number.octal'],
      [/(@digits)/, 'number'],
      [/'/, 'number', '@string_single'],
      [/'([^'\\]|\\.)*$/, 'number.invalid'],  // non-teminated string

      // delimiter: after number because of .\d floats
      [/[;,.]/, 'delimiter'],

      // strings
      [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
      [/"/, 'string', '@string_double'],
      [/@"/, 'string', '@string_verbatim'],
    ],

    whitespace: [
      [/[ \t\r\n]+/, ''],
      [/\/\*/, 'comment', '@comment'],
      [/\/\/.*$/, 'comment'],
    ],

    comment: [
      [/[^\/*]+/, 'comment'],
      [/\*\//, 'comment', '@pop'],
      [/[\/*]/, 'comment']
    ],

    string_double: [
      [/[^\\"]+/, 'string'],
      [/@escapes/, 'string.escape'],
      [/\\./, 'string.escape.invalid'],
      [/"/, 'string', '@pop']
    ],

    string_single: [
      [/[^\\']+/, 'number'],
      [/@escapes/, 'string.escape'],
      [/\\./, 'string.escape.invalid'],
      [/'/, 'number', '@pop']
    ],

    string_verbatim: [
      [/(?:[^"]|"")+/, 'string'],
      [/\\./, 'string.escape.invalid'],
      [/"/, 'string', '@pop']
    ]
  },
};

export default squirrelLanguage;
