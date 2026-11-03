# qulam

A type checker and normalizer, written in Haskell, for QuLam, a typed lambda
calculus based on quantum logic. QuLam corresponds to the natural deduction
system NQ for quantum logic.

The characteristic rule of QuLam is that a lambda abstraction
`\x : A -> M` is typed with its body `M` under the single declaration `x : A`
only: the body may not refer to any variable other than `x`. For example, the
K combinator `\x : 'a -> \y : 'b -> x` is not typable. Functions of several
arguments take one tuple argument instead.

## Paper

K. Tokuo, *A lambda calculus based on quantum logic*, arXiv:2610.34266 [cs.LO],
2026. <https://arxiv.org/abs/2610.34266>

## Building

With [Stack](https://docs.haskellstack.org/) (snapshot LTS 24.26):

```bash
stack build
```

```bash
stack run qulam-compiler -- tuple3.qlam
```

With Cabal:

```bash
cabal build
```

```bash
cabal run qulam-compiler -- tuple3.qlam
```

The only dependencies are `base` and `megaparsec`.

## Usage

`qulam-compiler FILE` reads a program, prints the source, the typing context
and the term, and, if the term is typable, its type and its normal form.
Without arguments it runs a small built-in demonstration.

A program consists of a typing context and a term:

```
context:
  x : 'a
  y : 'b
  z : 'c

term:
  fst (snd (x, y, z))
```

For this file (`tuple3.qlam`) the output ends with

```
[term]
  (fst (snd (x, (y, z))))
[type]
  'b
[nf]
  y
```

A term that violates the rule for lambda abstractions is reported as a type
error, for example `tau_unclosed_lambda.qlam`:

```
[type error]
Lambda is not closed; free variable: k
```

## Language

- Types: type variables `'a`, `Bot`, products `A * B`, functions `A -> B`.
- Terms: variables, `\x : A -> t`, application, pairs `(t, u)`, `fst t`,
  `snd t`, `eps A t`, `tau m n`, `eta t`, `theta t`, `zeta t`, `xi t`.
- Reductions: projections of pairs, beta reduction, and `eta (theta t)` to `t`.
  There are no reductions whose root is `zeta` or `xi`.
- Syntactic sugar, expanded by the parser: n-ary tuples `(t1, t2, t3)`
  (right-nested pairs) and `let x = e1 in e2`.

The full specification is in [`docs/qulam_spec_en.txt`](docs/qulam_spec_en.txt)
(a Japanese version is in [`docs/qulam_spec_ja.txt`](docs/qulam_spec_ja.txt)).
The `*.qlam` files in the top directory are examples.

## License

BSD-3-Clause. See [`LICENSE`](LICENSE).
