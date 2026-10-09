# Expected residual life.

Expected future lifetime of a life aged \\x\\, either over the whole
remaining lifespan or over a temporary period of \\n\\ years.

## Usage

``` r
exn(object, x, n, type = "curtate", fxt = 0.5, axOmega = 1 - fxt)
```

## Arguments

- object:

  A lifetable/actuarialtable object.

- x:

  Attained age

- n:

  Length (in years) of the period over which the expected lifetime is
  computed, i.e. a temporary expectation. Assumed omega - x + 1 (the
  whole remaining lifespan) whether missing.

- type:

  Either `"Tx"`, `"complete"` or `"continuous"` for the complete
  (continuous) future lifetime, `"Kx"` or `"curtate"` for the curtate
  future lifetime (can be abbreviated). Default is `"curtate"`.

- fxt:

  fraction of the year of age lived by those who die within the year,
  used by the `"complete"` branch (see
  [`Lxt`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)
  and
  [`Tx`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)).
  Defaults to `0.5` (uniform distribution of deaths); ignored by the
  `"curtate"` branch.

- axOmega:

  mean number of years lived in the last, open-ended age interval, used
  by the `"complete"` branch when the period reaches the last tabulated
  age (see
  [`Tx`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)).
  Defaults to `1 - fxt`, which reproduces the historical closed-interval
  behaviour.

## Value

A numeric value representing the expected life span.

## Details

For `type = "curtate"` the function returns the (temporary) curtate
expectation of life \$\$e\_{x:\overline{n}\|} = \sum\_{k=1}^{n} {}\_k
p_x ,\$\$ that is the expected number of complete future years lived by
(x) within the next \\n\\ years. With \\n\\ missing it is the curtate
expectation of life \\e_x = E\[K_x\]\\.

For `type = "complete"` the function returns the (temporary) complete
expectation of life \$\$\mathring{e}\_{x:\overline{n}\|} = \int_0^n
{}\_t p_x \\ dt = \frac{{}\_nL_x}{l_x} ,\$\$ evaluated under the uniform
distribution of deaths (UDD) assumption within each year, i.e. \\L_x =
l_x - 0.5 d_x\\ (see
[`Lxt`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)).
With \\n\\ missing it is \\\mathring{e}\_x = T_x / l_x = E\[T_x\]\\.

Under UDD the two quantities are related by
\\\mathring{e}\_{x:\overline{n}\|} = e\_{x:\overline{n}\|} + 0.5\\(1 -
{}\_n p_x)\\, which reduces to \\\mathring{e}\_x = e_x + 0.5\\ when
\\n\\ covers the whole remaining lifespan.

The last tabulated age \\\omega\\ is treated as a closed interval: those
alive at \\\omega\\ are assumed to die on average half a year later.
Published tables that close the table with an open interval (e.g.
\\L\_{\omega} = l\_{\omega}/m\_{\omega}\\, as in the NCHS tables) can
therefore show a slightly larger complete life expectancy.

## References

Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber,
H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.

## See also

[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md),
[`Tx`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md),
[`Lxt`](https://spedygiorgio.github.io/lifecontingencies/reference/other-demographic-functions.md)

## Author

Giorgio Alfredo Spedicato

## Examples

``` r
#loads and show
data(soa08Act)
#curtate expectation of life at birth
exn(object=soa08Act, x=0)
#> [1] 71.30789
#complete expectation of life at birth (curtate + 0.5 under UDD)
exn(object=soa08Act, x=0,type="complete")
#> [1] 71.80789
#temporary 20-year expectations at age 50
exn(object=soa08Act, x=50, n=20)
#> [1] 17.86296
exn(object=soa08Act, x=50, n=20, type="complete")
#> [1] 17.99338
```
