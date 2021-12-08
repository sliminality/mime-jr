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

::: notes
So next we'll talk about how we use matchers to decide where to apply transformers.
:::

## Synthesizing a guard

Given an input program $P$, synthesize Boolean guard $g$ to determine where to apply $T$.

$$\textsf{width} > 20 \lor \textsf{depth}^* > 4$$

\* not implemented yet

::: notes
At a high level, our synthesis problem looks like this: we have some transformation $T$. Now given an input program $P$, we want to synthesize a Boolean guard called $g$ that tells us where $g$ should be applied.

Here's an example of a guard we might synthesize: width > 20 or depth > 4, which says to apply the transformation anywhere the expression is more than 20 characters wide, or nested more than 4 levels deep.
:::

## Guard language

$$
\begin{aligned}
  n &::= \mathbb{N} \mid \mathrm{width} \mid \ldots \\
  b &::= \top \mid \bot \mid n > n \mid n = n \mid b \land b \mid b \lor b \mid \lnot b
\end{aligned}
$$

. . .

$$\textsf{width} > 20 \lor \textsf{depth} > 4$$

::: notes
We'll use a DSL to describe guards. There are two components: numerical-valued guards, which denote natural numbers, and Boolean-valued guards, which denote Booleans.

We have all of the usual Boolean constructs like True, False, And, Or, and Not. In addition, we have constant natural numbers and an operator called `width`, which describes the width of a an expression's concrete syntax in characters.
:::

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

::: notes
Now let's take a look at how to extract input-output examples for our synthesis problem. Consider the following program, which defines two variables: a string called `myList`, and a lambda called `add`. 

In addition, we have a function application of `foldr` to three arguments:
- `add`, which is defined above,
- the constant value `0`
- and the string `myList`, also defined above.

We can think of each argument as an input, with an output Boolean corresponding to whether or not the argument is defined elsewhere.
:::

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

::: notes
Here is a table summarizing each argument, whether or not it is `let`-bound, the corresponding expression, and the width of that expression. Note that for inline arguments we just take the width of the argument itself, but for bound arguments we take the width of the bound value.

These arguments form the input-output pairs for our synthesis problem.
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

::: notes
In order to formalize the notion of "width", let's take a step back and provide the denotational semantics for our guard DSL.

The denotation of Booleans, denoted with the subscript 2, is a function from concrete syntax trees to Boolean values.

We use the standard rules for Boolean algebra, lifting each operator into the meta-language.
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
\nden{\mathrm{width}}& {e} = \max \left( {\textsf{len}(e_i)} \right ),
 \; \forall e_i \in e \\
\nden{\mathrm{depth}}&{e} = \textsf{depth}(e) \\
\end{aligned}
$$
:::
:::

::: notes
For naturals, the denotation is a function from concrete syntax trees to natural numbers.

Numeric literals denote the corresponding literal value, while operators like `width` and `depth` denote properties of the given expression's concrete syntax tree.

Let's take a look at what that means.
:::

---

### $\newcommand\nden[2]{\llbracket #1 \rrbracket_\mathbb{N} \, #2}
\nden{\mathrm{width}} {e} = \max \left( {\textsf{len}(e_i)} \right ),
 \; \forall e_i \in e$


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

::: notes
Here we have the expression `add`, which spans multiple lines.

To take its width, we look at the length of the longest line, starting from the leftmost position of the first line. `width` is 15 characters long in its longest line, so the denotation of `width` on `add` is 15.
:::

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

::: notes
The final step is to synthesize a guard.

On the left we have an input program corresponding to the example we've been studying, and on the right we have some code to extract examples and synthesize a guard.
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

::: notes
If we run this code, we get the following output. First you can see the formatted program, followed by the input-output examples, and finally the synthesized guard.

Indeed, we can see that values with width greater than 10 are `let`-bound, while values with smaller widths are left inline. 

We can use this guard when applying the transformation to a new program, hoisting any inline argument wider than 10 characters.
:::

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
