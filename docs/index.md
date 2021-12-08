---
title: "<div style='font-size: 0.8em; margin-bottom: 0.5em;'><img src='mime-jr.png' style='height: 1.5em; position: relative; top: 55px;'/><span style='font-weight: 400;'>Mime Jr.</span><img src='https://www.serebii.net/pokedex-dp/evo/439.png' style='height: 1.5em; position: relative; top: 55px;'/></div>Code formatting<br>by example"
author:
    - Slim Lim
    - Justin Lubin
slideNumber: true
height: 850
width: 1250
transition: none
css: slim.css
navigationMode: "linear"
---

# Matching code style is hard

## Using `$` vs. nested parens

::: columns
::: column
```hs
f (g (h x))
```
:::

::: column
```hs
f $ g (h x)
```
:::
:::

## Pattern matching vs. `case` expressions

::: columns
::: column
```hs
f x (Node y l r) = ...
f x (Leaf y) = ...
```
:::

::: column
```hs
f x n = \case
  Node y l r -> ...
  Leaf y -> ...
```
:::
:::

## Inline expressions vs. `let`-binding

::: columns
::: column
```hs
foldr (\x y -> x + y) 0 xs
```
:::

::: column
```hs
let add = \x y -> x + y in
    foldr add 0 xs
```
:::
:::

## Knowing *when* to rewrite is hard

- Often depends on properties of the **concrete syntax tree** (CST)
    - e.g. line length, expression size, CST depth
- Even for simple properties, existing formatters require onerous configuration

. . .

> Can we use **example code** to produce rules for applying rewrites?

# Applying transformations *by example*

## Goal

Provide a framework for defining transformations based on CST properties

- **Given:** a program transformation $T$ and input program $P$
- **Synthesize:** a guard $g$ to determine when to apply $T$ in a new $P'$

## Example: `let`-hoisting

Opposite of inlining: **hoist** an expression into a definition to improve readability of the final expression

::: columns
::: column
```hs
foldr (\x y -> x + y) 0 xs
```
:::

::: column
```hs
let add = \x y -> x + y in
    foldr add 0 xs
```
:::
:::

## Defining transformations

A transformation $T$ rewrites a concrete syntax tree $C$.

- **Matchers** $C \to \mathbf{2}$ extract relevant pieces of syntax within a program
    - "Get all function arguments"
    - "Get all `let` bindings"
- **Transformer** $C \to C$ rewrites a program, using the matcher(s) to extract sub-expressions
    $$
    P \mapsto \texttt{let $v = x$ in $P[v/x]$}
    $$

# Synthesizing guards

## Synthesizing a guard

Given an input program $P$, synthesize Boolean guard $g$ to determine where to apply $T$.

$$\textsf{width} > 20 \lor \textsf{depth}^* > 4$$

\* not implemented yet

## Guard language

$$
\begin{aligned}
  n &::= \mathbb{N} \mid \mathrm{width} \mid \ldots \\
  b &::= \top \mid \bot \mid n > n \mid n = n \mid b \land b \mid b \lor b \mid \lnot b
\end{aligned}
$$

. . .

$$\textsf{width} > 20 \lor \textsf{depth} > 4$$

## Extracting examples

```hs
let myList = "abcdefghijklmnopqr" in
let add = \x, y -> plus x
                        y in
    foldr add
          0
          myList
```

Each program admits a set of pairs `(argument, isBound)`


## Extracting examples

::: columns
::: {.column width=45%}
```hs
let myList = "abcdefghijklmnopqr" in
let add = \x, y -> plus x
                        y in
    foldr add
          0
          myList
```
:::

::: {.column style="width: 55%; font-size: 0.8em"}
+----------+-------+------------------------+-------+
| Arg      |       | Expression             | Width |
+==========+=======+========================+=======+
| `add`    | ✅    | ```hs                  | 15    |
|          |       | \x, y -> plus x        |       |
|          |       |               y        |       |
|          |       | ```                    |       |
+----------+-------+------------------------+-------+
| `0`      | ❌    | `0`                    | 1     |
+----------+-------+------------------------+-------+
| `myList` | ✅    | `"abcdefghijklmnopqr"` | 20    |
+----------+-------+------------------------+-------+
| `x`      | ❌    | `x`                    | 1     |
+----------+-------+------------------------+-------+
| `y`      | ❌    | `y`                    | 1     |
+----------+-------+------------------------+-------+
:::
:::

## Denotational semantics for guards

::: {.columns style="font-size: 0.8em;"}
::: {.column width=55%}
> $\llbracket \cdot \rrbracket_\mathbf{2} \, : \mathrm{CST} \to \mathbf{2}$

$$
\newcommand\nden[2]{\llbracket #1 \rrbracket_\mathbb{N} \, #2}
\newcommand\bden[2]{\llbracket #1 \rrbracket_\mathbf{2} \, #2}
\begin{aligned}
\bden{\top} \, e &:= \top \\
\bden{\bot} \, e &:= \bot \\
\bden{\lnot b}{e} &:= \lnot \bden{b}\,e \\
\bden{n_1 > n_2} e &:= \nden{n_1}\,e > \nden{n_2} \, e \\
\bden{n_1 = n_2}{e} &:= \nden{n_1}\,e = \nden{n_2} \, e \\
\bden{b_1 \land b_2}{e} &:= \bden{b_1}\,e \land \bden{b_2}\,e \\
\bden{b_1 \lor b_2}{e} &:= \bden{b_1}\,e \lor \bden{b_2}\,e \\
\end{aligned}
$$
:::

::: {.column width=45%}
:::
:::

## Denotational semantics for guards

::: {.columns style="font-size: 0.8em;"}
::: {.column width=55%}
> $\llbracket \cdot \rrbracket_\mathbf{2} \, : \mathrm{CST} \to \mathbf{2}$

$$
\newcommand\nden[2]{\llbracket #1 \rrbracket_\mathbb{N} \, #2}
\newcommand\bden[2]{\llbracket #1 \rrbracket_\mathbf{2} \, #2}
\begin{aligned}
\bden{\top} \, e &:= \top \\
\bden{\bot} \, e &:= \bot \\
\bden{\lnot b}{e} &:= \lnot \bden{b}\,e \\
\bden{n_1 > n_2} e &:= \nden{n_1}\,e > \nden{n_2} \, e \\
\bden{n_1 = n_2}{e} &:= \nden{n_1}\,e = \nden{n_2} \, e \\
\bden{b_1 \land b_2}{e} &:= \bden{b_1}\,e \land \bden{b_2}\,e \\
\bden{b_1 \lor b_2}{e} &:= \bden{b_1}\,e \lor \bden{b_2}\,e \\
\end{aligned}
$$
:::

::: {.column width=45%}
> $\llbracket \cdot \rrbracket_\mathbb{N} \, : \mathrm{CST} \to \mathbb{N}$

$$
\newcommand\nden[2]{\llbracket #1 \rrbracket_\mathbb{N} \, #2}
\begin{aligned}
\nden{c}& e = c \in \mathbb{N} \\
\nden{\mathrm{width}}& {e} = \textsf{width}(e) \\
\nden{\mathrm{depth}}&{e} = \textsf{depth}(e) \\
\end{aligned}
$$
:::
:::

## $\llbracket \textsf{width} \rrbracket_\mathbb{N} \, \texttt{add} = 15$

```hs
let myList = "abcdefghijklmnopqr" in
--        vvvvvvvvvvvvvvv
let add = \x, y -> plus x
                        y in
--        ^^^^^^^^^^^^^^^ 
--        15 characters wide
    foldr add
          0
          myList
```

## Recap

1. Extract input-output examples
2. Define grammar for guards
3. **Synthesize guards**

## Synthesizing guards

Using **bottom-up enumerative synthesis** ([Armando Solar-Lezama](https://people.csail.mit.edu/asolar/SynthesisCourse/Lecture3.htm))

::: {.columns style="font-size: 0.8em;"}
::: column
```hs
test = Let (Name "myList")
           (Lit (LStr "abcdefghijklmnopqrstuvwxyz")) $
       Let (Name "add")
           (Lam [Name "x", Name "y"] $
                App (Var (Name "plus")) 
                    [Var (Name "x"), Var (Name "y")]) $
           App (Var (Name "foldr")) 
               [ Var (Name "add")
               , Lit (LNum 0)
               , Var (Name "myList") ]
```
:::

::: column
```hs
main = do
    res <- synth (getExamples test) 3
    case res of
      Just x -> print x
      _ -> print "No solution found"
```
:::
:::

## Synthesizing guards

```{.hs style="font-size: 0.8em;"}
******** Synthesizing guards for program:
let myList = "abcdefghijklmnopqrstuvwxyz" in
let add = \x, y -> plus x
                        y in
    foldr add
          0
          myList

******** Extracting examples:
[(True,15),(False,1),(True,28),(False,1),(False,1)]

******** Synthesized guard:
width > 10
```

\* real output


# Future work

- Implement other transformations
    - Using `$` vs. nested parens
    - Pattern-matching vs. `case` expressions
- Extend guard language to include more constructs
    - $\textsf{depth}$: nesting depth of an expression
    - $\textsf{area}$: takes the area of an expression, in characters
- Prove that rewrites are semantics-preserving
    - Would be onerous for authors of a transformation
