# Computational details of demographic functions

## Survival probabilities

For notation details, see (Bowers et al. 1997).

Using the well-known relation
${}_{s + u}p_{y} = {}_{u}p_{y + s} \times {}_{s}p_{y}$, we compute
${}_{t}p_{x}$ as for all
$x,t \in {\mathbb{R}}_{+}$$${}_{t}p_{x} = \frac{{}_{t + \epsilon_{x}}p_{\lfloor x\rfloor}}{{}_{\epsilon_{x}}p_{\lfloor x\rfloor}} = \frac{{}_{\lfloor u\rfloor}p_{\lfloor x\rfloor} \times {}_{\epsilon_{u}}p_{\lfloor x\rfloor + \lfloor u\rfloor}}{{}_{\epsilon_{x}}p_{\lfloor x\rfloor}}$$
with $\epsilon_{x} = x - \lfloor x\rfloor$, $u = t + \epsilon_{x}$ and
$\epsilon_{u} = u - \lfloor u\rfloor$.

Then we estimate ${}_{n}p_{m}$ as for all
$n,m \in {\mathbb{N}}$$${}_{n}p_{m} = \frac{l_{n + m}}{l_{m}}.$$

We interpolate fractional age probabilities by three classical
assumptions for all
$y \in \lbrack 0,1),m \in {\mathbb{N}}$$${}_{y}p_{m} = \begin{cases}
{1 - y\left( 1 - p_{m} \right)} & \text{if uniform distribution} \\
\left( p_{m} \right)^{y} & \text{if constant force} \\
\frac{p_{m}}{1 - (1 - y)\left( 1 - p_{m} \right)} & \text{if hyperbolic distribution} \\
 & 
\end{cases}$$

## Examples of non-integer times

``` r
library(lifecontingencies)
#> Package:  lifecontingencies
#> Authors:  Giorgio Alfredo Spedicato [aut, cre] (ORCID:
#>     <https://orcid.org/0000-0002-0315-8888>),
#>   Christophe Dutang [ctb] (ORCID:
#>     <https://orcid.org/0000-0001-6732-1501>),
#>   Reinhold Kainhofer [ctb] (ORCID:
#>     <https://orcid.org/0000-0002-7895-1311>),
#>   Kevin J Owens [ctb],
#>   Ernesto Schirmacher [ctb],
#>   Gian Paolo Clemente [ctb] (ORCID:
#>     <https://orcid.org/0000-0001-6795-4595>),
#>   Ivan Williams [ctb]
#> Version:  1.6.3
#> Date:     
#> BugReport: https://github.com/spedygiorgio/lifecontingencies/issues
data("soa08Act")
pXt <- Vectorize(lifecontingencies:::pxtold, "x")
pxT <- Vectorize(lifecontingencies:::pxtold, "t")
pxtvect <- pxt

z <- 1:6/3
#non integer time
cbind(t=z, pxtvect(soa08Act, x=100, t=z, fractional = "lin"), pxT(object=soa08Act, x=100, t=z, fractional = "lin"))
#>              t                    
#> [1,] 0.3333333 0.8639604 0.8639604
#> [2,] 0.6666667 0.7279208 0.7279208
#> [3,] 1.0000000 0.5918812 0.5918812
#> [4,] 1.3333333 0.5056079 0.5056079
#> [5,] 1.6666667 0.4193345 0.4193345
#> [6,] 2.0000000 0.3330612 0.3330612
cbind(t=z, pxtvect(soa08Act, x=100, t=z, fractional = "hyp"), pxT(object=soa08Act, x=100, t=z, fractional = "hyp"))
#>              t                    
#> [1,] 0.3333333 0.8131121 0.8131121
#> [2,] 0.6666667 0.6850791 0.6850791
#> [3,] 1.0000000 0.5918812 0.5918812
#> [4,] 1.3333333 0.4701083 0.4701083
#> [5,] 1.6666667 0.3898924 0.3898924
#> [6,] 2.0000000 0.3330612 0.3330612
cbind(t=z, pxtvect(soa08Act, x=100, t=z, fractional = "exp"), pxT(object=soa08Act, x=100, t=z, fractional = "exp"))
#>              t                    
#> [1,] 0.3333333 0.8396111 0.8396111
#> [2,] 0.6666667 0.7049468 0.7049468
#> [3,] 1.0000000 0.5918812 0.5918812
#> [4,] 1.3333333 0.4886498 0.4886498
#> [5,] 1.6666667 0.4034232 0.4034232
#> [6,] 2.0000000 0.3330612 0.3330612
```

## Examples of non-integer ages

``` r
x <- 50+0:6/6
#non-integer age
cbind(x=x, pxtvect(soa08Act, x=x, t=1, fractional = "lin"), pXt(object=soa08Act, x=x, t=1, fractional = "lin"))
#>             x                    
#> [1,] 50.00000 0.9940801 0.9940801
#> [2,] 50.16667 0.9939968 0.9939968
#> [3,] 50.33333 0.9939134 0.9939134
#> [4,] 50.50000 0.9938298 0.9938298
#> [5,] 50.66667 0.9937460 0.9937460
#> [6,] 50.83333 0.9936620 0.9936620
#> [7,] 51.00000 0.9935779 0.9935779
cbind(x=x, pxtvect(soa08Act, x=x, t=1, fractional = "hyp"), pXt(object=soa08Act, x=x, t=1, fractional = "hyp"))
#>             x                    
#> [1,] 50.00000 0.9940801 0.9940801
#> [2,] 50.16667 0.9939960 0.9939960
#> [3,] 50.33333 0.9939120 0.9939120
#> [4,] 50.50000 0.9938282 0.9938282
#> [5,] 50.66667 0.9937446 0.9937446
#> [6,] 50.83333 0.9936612 0.9936612
#> [7,] 51.00000 0.9935779 0.9935779
cbind(x=x, pxtvect(soa08Act, x=x, t=1, fractional = "exp"), pXt(object=soa08Act, x=x, t=1, fractional = "exp"))
#>             x                    
#> [1,] 50.00000 0.9940801 0.9940801
#> [2,] 50.16667 0.9939964 0.9939964
#> [3,] 50.33333 0.9939127 0.9939127
#> [4,] 50.50000 0.9938290 0.9938290
#> [5,] 50.66667 0.9937453 0.9937453
#> [6,] 50.83333 0.9936616 0.9936616
#> [7,] 51.00000 0.9935779 0.9935779
```

## Examples of large ages

``` r
x <- 135:145
#high-age
cbind(x=x, pxtvect(soa08Act, x=x, t=1), pXt(object=soa08Act, x=x, t=1))
#>         x                          
#>  [1,] 135 1.932519e-06 1.932519e-06
#>  [2,] 136 5.431077e-07 5.431077e-07
#>  [3,] 137 1.350422e-07 1.350422e-07
#>  [4,] 138 2.935883e-08 2.935883e-08
#>  [5,] 139 5.508989e-09 5.508989e-09
#>  [6,] 140 0.000000e+00 0.000000e+00
#>  [7,] 141 0.000000e+00 0.000000e+00
#>  [8,] 142 0.000000e+00 0.000000e+00
#>  [9,] 143 0.000000e+00 0.000000e+00
#> [10,] 144 0.000000e+00 0.000000e+00
#> [11,] 145 0.000000e+00 0.000000e+00
```

Bowers, N. L., D. A. Jones, H. U. Gerber, C. J. Nesbitt, and J. C.
Hickman. 1997. *Actuarial Mathematics, 2nd Edition*. Society of
Actuaries.
