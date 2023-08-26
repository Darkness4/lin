package lexer_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/Darkness4/lin/lexer"
	"github.com/Darkness4/lin/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNextToken_Number(t *testing.T) {
	for _, tt := range []struct {
		tok              token.Token
		src, tokens, err string
	}{
		// binaries
		{token.Integer, "0b0", "0b0", ""},
		{token.Integer, "0b1010", "0b1010", ""},
		{token.Integer, "0B1110", "0B1110", ""},

		{token.Integer, "0b", "0b", "binary literal has no digits"},
		{token.Integer, "0b0190", "0b0190", "invalid digit '9' in binary literal"},
		{token.Integer, "0b01a0", "0b01 a0", ""},

		{token.Float, "0b.", "0b.", "invalid radix point in binary literal"},
		{token.Float, "0b.1", "0b.1", "invalid radix point in binary literal"},
		{token.Float, "0b1.0", "0b1.0", "invalid radix point in binary literal"},
		{token.Float, "0b1e10", "0b1e10", "'e' exponent requires decimal mantissa"},
		{token.Float, "0b1P-1", "0b1P-1", "'P' exponent requires hexadecimal mantissa"},

		{token.Imaginary, "0b10i", "0b10i", ""},
		{token.Imaginary, "0b10.0i", "0b10.0i", "invalid radix point in binary literal"},

		// octals
		{token.Integer, "0o0", "0o0", ""},
		{token.Integer, "0o1234", "0o1234", ""},
		{token.Integer, "0O1234", "0O1234", ""},

		{token.Integer, "0o", "0o", "octal literal has no digits"},
		{token.Integer, "0o8123", "0o8123", "invalid digit '8' in octal literal"},
		{token.Integer, "0o1293", "0o1293", "invalid digit '9' in octal literal"},
		{token.Integer, "0o12a3", "0o12 a3", ""}, // only accept 0-9

		{token.Float, "0o.", "0o.", "invalid radix point in octal literal"},
		{token.Float, "0o.2", "0o.2", "invalid radix point in octal literal"},
		{token.Float, "0o1.2", "0o1.2", "invalid radix point in octal literal"},
		{token.Float, "0o1E+2", "0o1E+2", "'E' exponent requires decimal mantissa"},
		{token.Float, "0o1p10", "0o1p10", "'p' exponent requires hexadecimal mantissa"},

		{token.Imaginary, "0o10i", "0o10i", ""},
		{token.Imaginary, "0o10e0i", "0o10e0i", "'e' exponent requires decimal mantissa"},

		// 0-octals
		{token.Integer, "0", "0", ""},
		{token.Integer, "0123", "0123", ""},

		{token.Integer, "08123", "08123", "invalid digit '8' in octal literal"},
		{token.Integer, "01293", "01293", "invalid digit '9' in octal literal"},
		{token.Integer, "0F.", "0 F .", ""}, // only accept 0-9
		{token.Integer, "0123F.", "0123 F .", ""},
		{token.Integer, "0123456x", "0123456 x", ""},

		// decimals
		{token.Integer, "1", "1", ""},
		{token.Integer, "1234", "1234", ""},

		{token.Integer, "1f", "1 f", ""}, // only accept 0-9

		{token.Imaginary, "0i", "0i", ""},
		{token.Imaginary, "0678i", "0678i", ""},

		// decimal floats
		{token.Float, "0.", "0.", ""},
		{token.Float, "123.", "123.", ""},
		{token.Float, "0123.", "0123.", ""},

		{token.Float, ".0", ".0", ""},
		{token.Float, ".123", ".123", ""},
		{token.Float, ".0123", ".0123", ""},

		{token.Float, "0.0", "0.0", ""},
		{token.Float, "123.123", "123.123", ""},
		{token.Float, "0123.0123", "0123.0123", ""},

		{token.Float, "0e0", "0e0", ""},
		{token.Float, "123e+0", "123e+0", ""},
		{token.Float, "0123E-1", "0123E-1", ""},

		{token.Float, "0.e+1", "0.e+1", ""},
		{token.Float, "123.E-10", "123.E-10", ""},
		{token.Float, "0123.e123", "0123.e123", ""},

		{token.Float, ".0e-1", ".0e-1", ""},
		{token.Float, ".123E+10", ".123E+10", ""},
		{token.Float, ".0123E123", ".0123E123", ""},

		{token.Float, "0.0e1", "0.0e1", ""},
		{token.Float, "123.123E-10", "123.123E-10", ""},
		{token.Float, "0123.0123e+456", "0123.0123e+456", ""},

		{token.Float, "0e", "0e", "exponent has no digits"},
		{token.Float, "0E+", "0E+", "exponent has no digits"},
		{token.Float, "1e+f", "1e+ f", "exponent has no digits"},
		{token.Float, "0p0", "0p0", "'p' exponent requires hexadecimal mantissa"},
		{token.Float, "1.0P-1", "1.0P-1", "'P' exponent requires hexadecimal mantissa"},

		{token.Imaginary, "0.i", "0.i", ""},
		{token.Imaginary, ".123i", ".123i", ""},
		{token.Imaginary, "123.123i", "123.123i", ""},
		{token.Imaginary, "123e+0i", "123e+0i", ""},
		{token.Imaginary, "123.E-10i", "123.E-10i", ""},
		{token.Imaginary, ".123E+10i", ".123E+10i", ""},

		// hexadecimals
		{token.Integer, "0x0", "0x0", ""},
		{token.Integer, "0x1234", "0x1234", ""},
		{token.Integer, "0xcafef00d", "0xcafef00d", ""},
		{token.Integer, "0XCAFEF00D", "0XCAFEF00D", ""},

		{token.Integer, "0x", "0x", "hexadecimal literal has no digits"},
		{token.Integer, "0x1g", "0x1 g", ""},

		{token.Imaginary, "0xf00i", "0xf00i", ""},

		// hexadecimal floats
		{token.Float, "0x0p0", "0x0p0", ""},
		{token.Float, "0x12efp-123", "0x12efp-123", ""},
		{token.Float, "0xABCD.p+0", "0xABCD.p+0", ""},
		{token.Float, "0x.0189P-0", "0x.0189P-0", ""},
		{token.Float, "0x1.ffffp+1023", "0x1.ffffp+1023", ""},

		{token.Float, "0x.", "0x.", "hexadecimal literal has no digits"},
		{token.Float, "0x0.", "0x0.", "hexadecimal mantissa requires a 'p' exponent"},
		{token.Float, "0x.0", "0x.0", "hexadecimal mantissa requires a 'p' exponent"},
		{token.Float, "0x1.1", "0x1.1", "hexadecimal mantissa requires a 'p' exponent"},
		{token.Float, "0x1.1e0", "0x1.1e0", "hexadecimal mantissa requires a 'p' exponent"},
		{token.Float, "0x1.2gp1a", "0x1.2 gp1a", "hexadecimal mantissa requires a 'p' exponent"},
		{token.Float, "0x0p", "0x0p", "exponent has no digits"},
		{token.Float, "0xeP-", "0xeP-", "exponent has no digits"},
		{token.Float, "0x1234PAB", "0x1234P AB", "exponent has no digits"},
		{token.Float, "0x1.2p1a", "0x1.2p1 a", ""},

		{token.Imaginary, "0xf00.bap+12i", "0xf00.bap+12i", ""},

		// separators
		{token.Integer, "0b_1000_0001", "0b_1000_0001", ""},
		{token.Integer, "0o_600", "0o_600", ""},
		{token.Integer, "0_466", "0_466", ""},
		{token.Integer, "1_000", "1_000", ""},
		{token.Float, "1_000.000_1", "1_000.000_1", ""},
		{token.Imaginary, "10e+1_2_3i", "10e+1_2_3i", ""},
		{token.Integer, "0x_f00d", "0x_f00d", ""},
		{token.Float, "0x_f00d.0p1_2", "0x_f00d.0p1_2", ""},

		{token.Integer, "0b__1000", "0b__1000", "'_' must separate successive digits"},
		{token.Integer, "0o60___0", "0o60___0", "'_' must separate successive digits"},
		{token.Integer, "0466_", "0466_", "'_' must separate successive digits"},
		{token.Float, "1_.", "1_.", "'_' must separate successive digits"},
		{token.Float, "0._1", "0._1", "'_' must separate successive digits"},
		{token.Float, "2.7_e0", "2.7_e0", "'_' must separate successive digits"},
		{token.Imaginary, "10e+12_i", "10e+12_i", "'_' must separate successive digits"},
		{token.Integer, "0x___0", "0x___0", "'_' must separate successive digits"},
		{token.Float, "0x1.0_p0", "0x1.0_p0", "'_' must separate successive digits"},
	} {
		t.Run(tt.src, func(t *testing.T) {
			l := lexer.New(strings.NewReader(tt.src))
			for i, want := range strings.Split(tt.tokens, " ") {
				tok, lit, err := l.NextToken()

				// compute lit where for tokens where lit is not defined
				switch tok {
				case token.Period:
					lit = "."
				case token.Add:
					lit = "+"
				case token.Substract:
					lit = "-"
				}

				if i == 0 {
					if tt.err != "" {
						require.EqualError(t, errors.New(tt.err), err.Error())
					} else {
						assert.NoError(t, err)
						assert.Equal(t, tt.tok, tok)
					}
				}

				require.Equal(t, want, lit)
			}

			// make sure we read all
			tok, _, _ := l.NextToken()
			if tok == token.SemiColon {
				tok, _, _ = l.NextToken()
			}
			if tok != token.EOF {
				t.Errorf("%q: got %s; want EOF", tt.src, tok)
			}
		})
	}
}
