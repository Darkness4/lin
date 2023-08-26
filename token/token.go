package token

import (
	"strconv"
	"unicode"
)

type Token int

const (
	Illegal Token = iota
	EOF
	Comment

	// Identifiers and literals
	literal_beg
	Identifier // main
	Integer    // 12345
	Float      // 123.45
	Imaginary  // 123.45i
	Character  // 'abc'
	String     // "abc"
	literal_end

	// Operators
	operator_beg
	Assign // =

	Add       // +
	Substract // -
	Multiply  // *
	Quotient  // /
	Remainder // %

	AddAssign       // +=
	SubstractAssign // -=
	MultiplyAssign  // *=
	QuotientAssign  // /=
	RemainderAssign // %=

	And        // &
	Or         // |
	Xor        // ^
	ShiftLeft  // <<
	ShiftRight // >>

	AndAssign        // &=
	OrAssign         // |=
	XorAssign        // ^=
	ShiftLeftAssign  // <<=
	ShiftRightAssign // >>=

	LogicAnd  // &&
	LogicOr   // ||
	Increment // ++
	Decrement // --

	Equal   // ==
	Less    // <
	Greater // >
	Not     // !

	NotEqual     // !=
	LessEqual    // <=
	GreaterEqual // >=

	// Delimiters
	Comma     // ,
	Period    // .
	Ellipsis  // ...
	SemiColon // ;
	Colon     // :

	LeftParenthesis  // (
	RightParenthesis // )
	LeftBrace        // {
	RightBrace       // }
	LeftBracket      // [
	RightBracket     // ]
	operator_end

	// Keywords
	keyword_beg
	Function // fn
	Let

	Switch
	Case
	Default
	Continue
	Fallthrough
	Break

	If
	Else

	Defer

	Goto
	Return
	keyword_end
)

var tokens = [...]string{
	Illegal: "ILLEGAL",

	EOF:     "EOF",
	Comment: "COMMENT",

	Identifier: "IDENT",
	Integer:    "INT",
	Float:      "FLOAT",
	Imaginary:  "IMAG",
	Character:  "CHAR",
	String:     "STRING",

	Assign: "=",

	Add:       "+",
	Substract: "-",
	Multiply:  "*",
	Quotient:  "/",
	Remainder: "%",

	AddAssign:       "+=",
	SubstractAssign: "-=",
	MultiplyAssign:  "*=",
	QuotientAssign:  "/=",
	RemainderAssign: "%=",

	And:        "&",
	Or:         "|",
	Xor:        "^",
	ShiftLeft:  "<<",
	ShiftRight: ">>",

	AndAssign:        "&=",
	OrAssign:         "|=",
	XorAssign:        "^=",
	ShiftLeftAssign:  "<<=",
	ShiftRightAssign: ">>=",

	LogicAnd:  "&&",
	LogicOr:   "||",
	Increment: "++",
	Decrement: "--",

	Equal:   "==",
	Less:    "<",
	Greater: ">",
	Not:     "!",

	NotEqual:     "!=",
	LessEqual:    "<=",
	GreaterEqual: ">=",

	Comma:     ",",
	Period:    ".",
	Ellipsis:  "...",
	SemiColon: ";",
	Colon:     ":",

	LeftParenthesis:  "(",
	RightParenthesis: ")",
	LeftBrace:        "{",
	RightBrace:       "}",
	LeftBracket:      "[",
	RightBracket:     "]",

	Function: "fn",
	Let:      "let",

	Switch:      "switch",
	Case:        "case",
	Continue:    "continue",
	Fallthrough: "fallthrough",
	Break:       "break",
	Default:     "default",

	If:   "if",
	Else: "else",

	Defer: "defer",

	Goto: "goto",
}

// String returns the string corresponding to the token tok.
// For operators, delimiters, and keywords the string is the actual
// token character sequence (e.g., for the token Add, the string is
// "+"). For all other tokens the string corresponds to the token
// constant name (e.g. for the token Ident, the string is "IDENT").
func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

// Lookup maps an identifier to its keyword token or IDENT (if not a keyword).
func Lookup(ident string) Token {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return Identifier
}

// A set of constants for precedence-based expression parsing.
// Non-operators have lowest precedence, followed by operators
// starting with precedence 1 up to unary operators. The highest
// precedence serves as "catch-all" precedence for selector,
// indexing, and other operator and delimiter tokens.
const (
	LowestPrec  = 0 // non-operators
	UnaryPrec   = 6
	HighestPrec = 7
)

// Precedence returns the operator precedence of the binary
// operator op. If op is not a binary operator, the result
// is LowestPrecedence.
func (op Token) Precedence() int {
	switch op {
	case LogicOr:
		return 1
	case LogicAnd:
		return 2
	case Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual:
		return 3
	case Add, Substract, Or, Xor:
		return 4
	case Multiply, Quotient, Remainder, ShiftLeft, ShiftRight, And:
		return 5
	}
	return LowestPrec
}

// Predicates

// IsLiteral returns true for tokens corresponding to identifiers
// and basic type literals; it returns false otherwise.
func (tok Token) IsLiteral() bool { return literal_beg < tok && tok < literal_end }

// IsOperator returns true for tokens corresponding to operators and
// delimiters; it returns false otherwise.
func (tok Token) IsOperator() bool {
	return (operator_beg < tok && tok < operator_end)
}

// IsKeyword returns true for tokens corresponding to keywords;
// it returns false otherwise.
func (tok Token) IsKeyword() bool { return keyword_beg < tok && tok < keyword_end }

var keywords map[string]Token

func init() {
	keywords = make(map[string]Token, keyword_end-(keyword_beg+1))
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

// IsKeyword reports whether name is a Go keyword, such as "func" or "return".
func IsKeyword(name string) bool {
	_, ok := keywords[name]
	return ok
}

// IsIdentifier reports whether name is a Go identifier, that is, a non-empty
// string made up of letters, digits, and underscores, where the first character
// is not a digit. Keywords are not identifiers.
func IsIdentifier(name string) bool {
	if name == "" || IsKeyword(name) {
		return false
	}
	for i, c := range name {
		if !unicode.IsLetter(c) && c != '_' && (i == 0 || !unicode.IsDigit(c)) {
			return false
		}
	}
	return true
}
