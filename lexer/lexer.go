package lexer

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"unicode"

	"github.com/Darkness4/lin/token"
)

var (
	invalidDigitError = errors.New("invalid digit")
)

const (
	eof = -1
)

type Lexer struct {
	input      *bufio.Reader
	ch         rune // Current char
	insertSemi bool // insert a semi colon before next newline
}

func New(r io.Reader) *Lexer {
	l := &Lexer{
		input:      bufio.NewReader(r),
		insertSemi: false,
	}
	if nexterr := l.next(); nexterr != nil {
		panic(nexterr)
	}
	return l
}

func (l *Lexer) NextToken() (tok token.Token, lit string, err error) {
	if err = l.skipWhitespace(); err != nil {
		tok = token.Illegal
		return tok, "", err
	}

	insertSemi := false
	switch ch := l.ch; {
	case unicode.IsLetter(ch):
		lit, err = l.scanIdentifier()
		if err != nil {
			return token.Illegal, lit, err
		}
		if len(lit) > 1 {
			// keywords are longer than one letter - avoid lookup otherwise
			tok = token.Lookup(lit)
			switch tok {
			case token.Identifier, token.Break, token.Continue, token.Fallthrough, token.Return:
				insertSemi = true
			}
		} else {
			insertSemi = true
			tok = token.Identifier
		}
	case (unicode.IsDigit(ch) || ch == '.' && unicode.IsDigit(l.peek())):
		insertSemi = true
		tok, lit, err = l.scanNumber()
	default:
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, "", nexterr
		}
		switch ch {
		case eof:
			if l.insertSemi {
				l.insertSemi = false // EOF consumed
				return token.SemiColon, "\n", nil
			}
			tok = token.EOF
		case '\n':
			// we only reach here if l.insertSemi was
			// set in the first place and exited early
			// from s.skipWhitespace()
			l.insertSemi = false // newline consumed
			return token.SemiColon, "\n", nil
		case '"':
			insertSemi = true
			tok = token.String
			lit, err = l.scanString()
			if err != nil {
				return token.Illegal, "", err
			}
		case '\'':
			insertSemi = true
			tok = token.Character
			lit, err = l.scanRune()
			if err != nil {
				return token.Illegal, "", err
			}
		case '`':
			insertSemi = true
			tok = token.String
			lit, err = l.scanRawString()
			if err != nil {
				return token.Illegal, "", err
			}
		case ':':
			tok = token.Colon
		case '.':
			// fractions starting with a '.' are handled by outer switch
			tok = token.Period
			if l.ch == '.' && l.peek() == '.' {
				lit += string(l.ch)
				if nexterr := l.next(); nexterr != nil {
					return token.Illegal, "", nexterr
				}
				lit += string(l.ch)
				if nexterr := l.next(); nexterr != nil { // consume last '.'
					return token.Illegal, "", nexterr
				}
				tok = token.Ellipsis
			}
		case ',':
			tok = token.Comma
		case ';':
			tok = token.SemiColon
			lit = ";"
		case '(':
			tok = token.LeftParenthesis
		case ')':
			insertSemi = true
			tok = token.RightParenthesis
		case '[':
			tok = token.LeftBracket
		case ']':
			insertSemi = true
			tok = token.RightBracket
		case '{':
			tok = token.LeftBracket
		case '}':
			insertSemi = true
			tok = token.RightBracket
		case '+':
			tok, err = l.switch3(token.Add, token.AddAssign, '+', token.Increment)
			if err != nil {
				return tok, "", err
			}
			if tok == token.Increment {
				insertSemi = true
			}
		case '-':
			tok, err = l.switch3(token.Substract, token.SubstractAssign, '-', token.Decrement)
			if err != nil {
				return tok, "", err
			}
			if tok == token.Decrement {
				insertSemi = true
			}
		case '*':
			tok, err = l.switch2(token.Multiply, token.MultiplyAssign)
			if err != nil {
				return tok, "", err
			}
		case '/':
			if l.ch == '/' || l.ch == '*' {
				// comment
				comment, err := l.scanComment()
				if err != nil {
					return token.Illegal, comment, err
				}
				// TODO: there may be some work here...
				insertSemi = l.insertSemi // preserve insertSemi info
				tok = token.Comment
				lit = comment
			} else {
				// division
				tok, err = l.switch2(token.Quotient, token.QuotientAssign)
				if err != nil {
					return tok, "", err
				}
			}
		case '%':
			tok, err = l.switch2(token.Remainder, token.RemainderAssign)
			if err != nil {
				return tok, "", err
			}
		case '^':
			tok, err = l.switch2(token.Xor, token.XorAssign)
			if err != nil {
				return tok, "", err
			}
		case '<':
			tok, err = l.switch4(
				token.Less,
				token.LessEqual,
				'<',
				token.ShiftLeft,
				token.ShiftLeftAssign,
			)
			if err != nil {
				return tok, "", err
			}
		case '>':
			tok, err = l.switch4(
				token.Greater,
				token.GreaterEqual,
				'>',
				token.ShiftRight,
				token.ShiftRightAssign,
			)
			if err != nil {
				return tok, "", err
			}
		case '=':
			tok, err = l.switch2(token.Assign, token.Equal)
			if err != nil {
				return tok, "", err
			}
		case '!':
			tok, err = l.switch2(token.Not, token.NotEqual)
			if err != nil {
				return tok, "", err
			}
		case '&':
			tok, err = l.switch3(token.And, token.AndAssign, '&', token.LogicAnd)
			if err != nil {
				return tok, "", err
			}
		case '|':
			tok, err = l.switch3(token.Or, token.OrAssign, '|', token.LogicOr)
			if err != nil {
				return tok, "", err
			}
		default:
			insertSemi = l.insertSemi // preserve insertSemi info
			tok = token.Illegal
			lit = string(l.ch)
		}
	}
	l.insertSemi = insertSemi
	return tok, lit, err
}

func (l *Lexer) next() (err error) {
	char, _, err := l.input.ReadRune()
	if err == io.EOF {
		l.ch = eof
		return nil
	}
	l.ch = char
	return
}

func (l *Lexer) skipWhitespace() error {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' && !l.insertSemi || l.ch == '\r' {
		if nexterr := l.next(); nexterr != nil {
			return nexterr
		}
	}
	return nil
}

func (l *Lexer) peek() rune {
	b, err := l.input.Peek(1)
	if err != nil {
		return 0
	}
	return rune(b[0])
}

func (l *Lexer) scanIdentifier() (lit string, err error) {
	for i := 0; ; i++ {
		if !unicode.IsLetter(l.ch) && l.ch != '_' && (i == 0 || !unicode.IsDigit(l.ch)) {
			break
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return lit, nexterr
		}
	}

	return lit, nil
}

func (l *Lexer) scanNumber() (tok token.Token, lit string, firsterr error) {
	base := 10        // number base
	prefix := rune(0) // one of 0 (decimal), '0' (0-octal), 'x', 'o', or 'b'
	digsep := 0       // bit 0: digit present, bit 1: '_' present

	// integer part
	if l.ch != '.' {
		tok = token.Integer

		// 0x/0o/0b prefixes
		if l.ch == '0' {
			lit += string(l.ch)
			if nexterr := l.next(); nexterr != nil {
				return token.Illegal, lit, nexterr
			}
			switch unicode.ToLower(l.ch) {
			case 'x':
				lit += string(l.ch)
				if nexterr := l.next(); nexterr != nil {
					return token.Illegal, lit, nexterr
				}
				base, prefix = 16, 'x'
			case 'o':
				lit += string(l.ch)
				if nexterr := l.next(); nexterr != nil {
					return token.Illegal, lit, nexterr
				}
				base, prefix = 8, 'o'
			case 'b':
				lit += string(l.ch)
				if nexterr := l.next(); nexterr != nil {
					return token.Illegal, lit, nexterr
				}
				base, prefix = 2, 'b'
			default:
				base, prefix = 8, '0'
				digsep = 1 // leading 0
			}
		}
		litDigits, ds, err := l.scanDigits(base)
		firsterr = errorIfEmpty(firsterr, err)
		lit += litDigits
		digsep |= ds
	}

	// fractional part
	if l.ch == '.' {
		tok = token.Float
		if prefix == 'o' || prefix == 'b' {
			firsterr = errorIfEmpty(
				firsterr,
				fmt.Errorf("invalid radix point in %s", litname(prefix)),
			)
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, lit, nexterr
		}
		litDigits, ds, err := l.scanDigits(base)
		firsterr = errorIfEmpty(firsterr, err)
		lit += litDigits
		digsep |= ds
	}

	if digsep&1 == 0 {
		firsterr = errorIfEmpty(firsterr, fmt.Errorf("%s has no digits", litname(prefix)))
	}

	// exponent
	if e := unicode.ToLower(l.ch); e == 'e' || e == 'p' {
		switch {
		case e == 'e' && prefix != 0 && prefix != '0':
			firsterr = errorIfEmpty(
				firsterr,
				fmt.Errorf("%q exponent requires decimal mantissa", l.ch),
			)
		case e == 'p' && prefix != 'x':
			firsterr = errorIfEmpty(
				firsterr,
				fmt.Errorf("%q exponent requires hexadecimal mantissa", l.ch),
			)
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, lit, nexterr
		}
		tok = token.Float
		if l.ch == '+' || l.ch == '-' {
			lit += string(l.ch)
			if nexterr := l.next(); nexterr != nil {
				return token.Illegal, lit, nexterr
			}
		}
		litDigits, ds, err := l.scanDigits(10)
		firsterr = errorIfEmpty(firsterr, err)
		lit += litDigits
		digsep |= ds
		if ds&1 == 0 {
			firsterr = errorIfEmpty(firsterr, errors.New("exponent has no digits"))
		}
	} else if prefix == 'x' && tok == token.Float {
		firsterr = errorIfEmpty(firsterr, errors.New("hexadecimal mantissa requires a 'p' exponent"))
	}

	// suffix 'i'
	if l.ch == 'i' {
		lit += string(l.ch)
		tok = token.Imaginary
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, lit, nexterr
		}
	}

	if tok != token.Integer && errors.Unwrap(firsterr) == invalidDigitError {
		firsterr = nil
	}

	if digsep&2 != 0 {
		if i := invalidSep(lit); i >= 0 {
			firsterr = errorIfEmpty(firsterr, errors.New("'_' must separate successive digits"))
		}
	}

	return tok, lit, firsterr
}

// scanDigits accepts the sequence { digit | '_' }.
// If base <= 10, digits accepts any decimal digit but records
// the offset (relative to the source start) of a digit >= base
// in *invalid, if *invalid < 0.
// digits returns a bitset describing whether the sequence contained
// digits (bit 0 is set), or separators '_' (bit 1 is set).
func (l *Lexer) scanDigits(base int) (lit string, digsep int, err error) {
	var invalid error
	if base <= 10 {
		max := rune('0' + base)
		for unicode.IsDigit(l.ch) || l.ch == '_' {
			ds := 1
			if l.ch == '_' {
				ds = 2
			} else if l.ch >= max {
				invalid = fmt.Errorf("%w '%c' in %s", invalidDigitError, l.ch, litnameFromBase(base))
			}
			digsep |= ds
			lit += string(l.ch)
			if nexterr := l.next(); nexterr != nil {
				return lit, 0, nexterr
			}
		}
	} else {
		for unicode.Is(unicode.Hex_Digit, l.ch) || l.ch == '_' {
			ds := 1
			if l.ch == '_' {
				ds = 2
			}
			digsep |= ds
			lit += string(l.ch)
			if nexterr := l.next(); nexterr != nil {
				return lit, 0, nexterr
			}
		}
	}
	if invalid != nil {
		err = invalid
	}
	return
}

func (l *Lexer) scanRawString() (lit string, err error) {
	hasCR := false
	for {
		ch := l.ch
		if ch < 0 {
			return lit, errors.New("raw string literal not terminated")
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return lit, nexterr
		}
		if ch == '`' {
			break
		}
		if ch == '\r' {
			hasCR = true
		}
	}

	if hasCR {
		lit = string(stripCR([]byte(lit), false))
	}

	return lit, nil
}

func (l *Lexer) scanString() (lit string, err error) {
	for {
		ch := l.ch
		if ch == '\n' || ch < 0 {
			return "", errors.New("string literal not terminated")
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return "", nexterr
		}
		if ch == '"' {
			break
		}
		if ch == '\\' {
			l.scanEscape('"')
		}
	}

	return lit, nil
}

func (l *Lexer) scanRune() (lit string, err error) {
	valid := true
	n := 0
	for {
		ch := l.ch
		if ch == '\n' || ch < 0 {
			// only report error if we don't have one already
			if valid {
				return lit, errors.New("rune literal not terminated")
			}
			break
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return lit, nexterr
		}
		// closing quote
		if ch == '\'' {
			break
		}
		n++
		// break line
		if ch == '\\' {
			// look for closing quote
			if ok, err := l.scanEscape('\''); !ok {
				valid = false
			} else if err != nil {
				return lit, err
			}
			// continue to read to closing quote
		}
	}

	if valid && n != 1 {
		return lit, errors.New("illegal rune literal")
	}

	return lit, nil
}

// scanEscape parses an escape sequence where rune is the accepted
// escaped quote. In case of a syntax error, it stops at the offending
// character (without consuming it) and returns false. Otherwise
// it returns true.
func (l *Lexer) scanEscape(quote rune) (bool, error) {
	// TODO: return lit
	var n int
	var base, max uint32
	switch l.ch {
	case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', quote:
		if nexterr := l.next(); nexterr != nil {
			return false, nexterr
		}
		return true, nil
	case '0', '1', '2', '3', '4', '5', '6', '7':
		n, base, max = 3, 8, 255
	case 'x':
		if nexterr := l.next(); nexterr != nil {
			return false, nexterr
		}
		n, base, max = 2, 16, 255
	case 'u':
		if nexterr := l.next(); nexterr != nil {
			return false, nexterr
		}
		n, base, max = 4, 16, unicode.MaxRune
	case 'U':
		if nexterr := l.next(); nexterr != nil {
			return false, nexterr
		}
		n, base, max = 8, 16, unicode.MaxRune
	default:
		msg := "unknown escape sequence"
		if l.ch < 0 {
			msg = "escape sequence not terminated"
		}
		return false, errors.New(msg)
	}

	var x uint32
	for n > 0 {
		d := uint32(digitVal(l.ch))
		if d >= base {
			msg := fmt.Sprintf("illegal character %#U in escape sequence", l.ch)
			if l.ch < 0 {
				msg = "escape sequence not terminated"
			}
			return false, errors.New(msg)
		}
		x = x*base + d
		if nexterr := l.next(); nexterr != nil {
			return false, nexterr
		}
		n--
	}

	if x > max || 0xD800 <= x && x < 0xE000 {
		return false, errors.New("escape sequence is invalid Unicode code point")
	}

	return true, nil
}

// scanComment returns the text of the comment and (if nonzero)
// the offset of the first newline within it, which implies a
// /*...*/ comment.
func (l *Lexer) scanComment() (lit string, err error) {
	numCR := 0

	if l.ch == '/' {
		//-style comment
		// (the final '\n' is not considered part of the comment)
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return lit, nexterr
		}
		for l.ch != '\n' && l.ch >= 0 {
			if l.ch == '\r' {
				numCR++
			}
			lit += string(l.ch)
			if nexterr := l.next(); nexterr != nil {
				return lit, nexterr
			}
			lit += string(l.ch)
		}
		goto exit
	}

	/*-style comment */
	lit += string(l.ch)
	if nexterr := l.next(); nexterr != nil {
		return lit, nexterr
	}
	for l.ch >= 0 {
		ch := l.ch
		if ch == '\r' {
			numCR++
		}
		lit += string(l.ch)
		if nexterr := l.next(); nexterr != nil {
			return lit, nexterr
		}
		// closing comment
		if ch == '*' && l.ch == '/' {
			lit += string(l.ch)
			if nexterr := l.next(); nexterr != nil {
				return lit, nexterr
			}
			goto exit
		}
	}

	return lit, errors.New("comment not terminated")

exit:
	// On Windows, a (//-comment) line may end in "\r\n".
	// Remove the final '\r' before analyzing the text for
	// line directives (matching the compiler). Remove any
	// other '\r' afterwards (matching the pre-existing be-
	// havior of the scanner).
	if numCR > 0 && len(lit) >= 2 && lit[1] == '/' && lit[len(lit)-1] == '\r' {
		lit = lit[:len(lit)-1]
		numCR--
	}

	if numCR > 0 {
		lit = string(stripCR([]byte(lit), lit[1] == '*'))
	}

	return lit, err
}

// invalidSep returns the index of the first invalid separator in x, or -1.
func invalidSep(x string) int {
	x1 := ' ' // prefix char, we only care if it's 'x'
	d := '.'  // digit, one of '_', '0' (a digit), or '.' (anything else)
	i := 0

	// a prefix counts as a digit
	if len(x) >= 2 && x[0] == '0' {
		x1 = unicode.ToLower(rune(x[1]))
		if x1 == 'x' || x1 == 'o' || x1 == 'b' {
			d = '0'
			i = 2
		}
	}

	// mantissa and exponent
	for ; i < len(x); i++ {
		p := d // previous digit
		d = rune(x[i])
		switch {
		case d == '_':
			if p != '0' {
				return i
			}
		case unicode.IsDigit(d) || x1 == 'x' && unicode.Is(unicode.Hex_Digit, d):
			d = '0'
		default:
			if p == '_' {
				return i - 1
			}
			d = '.'
		}
	}
	if d == '_' {
		return len(x) - 1
	}

	return -1
}

func litname(prefix rune) string {
	switch prefix {
	case 'x':
		return "hexadecimal literal"
	case 'o', '0':
		return "octal literal"
	case 'b':
		return "binary literal"
	}
	return "decimal literal"
}

func litnameFromBase(base int) string {
	switch base {
	case 16:
		return "hexadecimal literal"
	case 8:
		return "octal literal"
	case 2:
		return "binary literal"
	}
	return "decimal literal"
}

// Helper functions for scanning multi-byte tokens such as >> += >>= .
// Different routines recognize different length tok_i based on matches
// of ch_i. If a token ends in '=', the result is tok1 or tok3
// respectively. Otherwise, the result is tok0 if there was no other
// matching character, or tok2 if the matching character was ch2.

func (l *Lexer) switch2(tok0, tok1 token.Token) (token.Token, error) {
	if l.ch == '=' {
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, nexterr
		}
		return tok1, nil
	}
	return tok0, nil
}

func (l *Lexer) switch3(tok0, tok1 token.Token, ch2 rune, tok2 token.Token) (token.Token, error) {
	if l.ch == '=' {
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, nexterr
		}
		return tok1, nil
	}
	if l.ch == ch2 {
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, nexterr
		}
		return tok2, nil
	}
	return tok0, nil
}

func (l *Lexer) switch4(
	tok0, tok1 token.Token,
	ch2 rune,
	tok2, tok3 token.Token,
) (token.Token, error) {
	if l.ch == '=' {
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, nexterr
		}
		return tok1, nil
	}
	if l.ch == ch2 {
		if nexterr := l.next(); nexterr != nil {
			return token.Illegal, nexterr
		}
		if l.ch == '=' {
			if nexterr := l.next(); nexterr != nil {
				return token.Illegal, nexterr
			}
			return tok3, nil
		}
		return tok2, nil
	}
	return tok0, nil
}

func digitVal(ch rune) int {
	switch {
	case '0' <= ch && ch <= '9':
		return int(ch - '0')
	case 'a' <= unicode.ToLower(ch) && unicode.ToLower(ch) <= 'f':
		return int(unicode.ToLower(ch) - 'a' + 10)
	}
	return 16 // larger than any legal digit val
}

func stripCR(b []byte, comment bool) []byte {
	c := make([]byte, len(b))
	i := 0
	for j, ch := range b {
		// In a /*-style comment, don't strip \r from *\r/ (incl.
		// sequences of \r from *\r\r...\r/) since the resulting
		// */ would terminate the comment too early unless the \r
		// is immediately following the opening /* in which case
		// it's ok because /*/ is not closed yet (issue #11151).
		if ch != '\r' ||
			comment && i > len("/*") && c[i-1] == '*' && j+1 < len(b) && b[j+1] == '/' {
			c[i] = ch
			i++
		}
	}
	return c[:i]
}

func errorIfEmpty(err error, newerr error) error {
	if err == nil {
		return newerr
	}
	return err
}
