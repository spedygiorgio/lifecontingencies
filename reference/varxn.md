# Variance and standard deviation of the future lifetime

`varxn` and `sdxn` return the variance and the standard deviation of the
future lifetime of a life aged \\x\\, as the natural second-moment
companions of
[`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)
(which returns the mean).

## Usage

``` r
varxn(object, x, n, type = "Kx")

sdxn(object, x, n, type = "Kx")
```

## Arguments

- object:

  A `lifetable` or `actuarialtable` object.

- x:

  Attained age. Defaults to `0`.

- n:

  Length (in years) of a temporary period. If missing, the whole
  remaining lifespan is used.

- type:

  Either `"Kx"`/`"curtate"` for the curtate future lifetime \\K_x\\ or
  `"Tx"`/`"complete"`/`"continuous"` for the complete future lifetime
  \\T_x\\ (can be abbreviated). Default is `"Kx"`.

## Value

A numeric value: the variance (`varxn`) or standard deviation (`sdxn`)
of the (temporary) future lifetime.

## Details

For the curtate future lifetime the (temporary) variance is computed
exactly from the life table as \$\$\mathrm{Var}(\min(K_x, n)) =
\sum\_{k=1}^{n}(2k-1)\\{}\_k p_x - \left(\sum\_{k=1}^{n}{}\_k
p_x\right)^2 .\$\$

For the complete future lifetime the variance is returned only for the
whole remaining lifespan (`n` missing), using the
uniform-distribution-of-deaths relation \\\mathrm{Var}(T_x) =
\mathrm{Var}(K_x) + 1/12\\ (because, under UDD, \\T_x = K_x + U\\ with
\\U\sim\mathrm{Unif}(0,1)\\ independent of \\K_x\\). A temporary
complete variance is not provided; use `type = "Kx"` for a temporary
period.

## References

Dickson, D.C.M., Hardy, M.R., Waters, H.R. (2013), Actuarial Mathematics
for Life Contingent Risks, 2nd ed., Cambridge University Press.

## See also

[`exn`](https://spedygiorgio.github.io/lifecontingencies/reference/exn.md)

## Author

Giorgio Alfredo Spedicato

## Examples

``` r
data(soa08Act)
# variance and sd of the curtate future lifetime at birth
varxn(soa08Act, x = 0)
#> [1] 382.6894
sdxn(soa08Act, x = 0)
#> [1] 19.56245
# complete future lifetime: Var(T) = Var(K) + 1/12
varxn(soa08Act, x = 65, type = "complete")
#> [1] 68.42574
# temporary (20-year) curtate future lifetime at age 40
varxn(soa08Act, x = 40, n = 20)
#> [1] 10.61487
```
