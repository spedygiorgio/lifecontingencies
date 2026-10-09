# Demographic analysis with the lifecontingencies package

## Scope

This vignette collects the *demographic* functions of the
**lifecontingencies** package: the quantities that describe the
mortality of a cohort through a life table, independently of any
interest rate. It complements the introductory vignette (which focuses
on the financial and actuarial functions) and the multiple-decrement
vignette.

All of the examples below run on life tables that can be reconstructed
exactly, so the numbers can be checked against the standard actuarial
and demographic literature ([Bowers et al.
1997](#ref-bowers1997actuarial); [Dickson et al.
2009](#ref-dickson2009actuarial); [Keyfitz and Caswell
2005](#ref-keyfitz2005applied)).

## Building and inspecting a life table

A `lifetable` object stores a vector of ages `x` and the survivorship
column `lx`. The illustrative Society of Actuaries table bundled with
the package is an `actuarialtable` (a `lifetable` that additionally
carries an interest rate), so every demographic function below works on
it unchanged.

``` r
data(soa08Act)
getOmega(soa08Act)          # last attainable age
#> [1] 140
soa08Act@lx[1:5]            # survivorship at the first ages
#> [1] 100000.00  97957.83  97826.26  97706.55  97596.74
```

Printing a `lifetable` tabulates the one-year survival probability
together with the person-years `Lx`, the total time lived `Tx` and the
expectation of life `ex`. The `print` method accepts optional arguments
(discussed below) and returns the displayed `data.frame` invisibly, so
it can be captured for further use:

``` r
lt <- new("lifetable", x = 0:5, lx = c(1000, 990, 975, 940, 880, 790),
          name = "toy")
df <- print(lt)
#> Life table toy 
#> 
#>   x   lx        px    Lx     Tx        ex
#> 1 0 1000 0.9900000 995.0 5075.0 4.5750000
#> 2 1  990 0.9848485 982.5 4080.0 3.6212121
#> 3 2  975 0.9641026 957.5 3097.5 2.6769231
#> 4 3  940 0.9361702 910.0 2140.0 1.7765957
#> 5 4  880 0.8977273 835.0 1230.0 0.8977273
```

## Core demographic functions

The basic one-year and `t`-year quantities are computed directly from
the `lx` column.

``` r
pxt(soa08Act, x = 65, t = 1)     # 1-year survival probability
#> [1] 0.9786797
qxt(soa08Act, x = 65, t = 5)     # 5-year death probability
#> [1] 0.1218228
dxt(soa08Act, x = 65, t = 10)    # deaths between 65 and 75
#> [1] 21378.83
Lxt(soa08Act, x = 65, t = 1)     # person-years lived in [65, 66)
#> [1] 74536.5
mxt(soa08Act, x = 65, t = 1)     # central mortality rate
#> [1] 0.02155
Tx(soa08Act, x = 65)             # total years lived from age 65
#> [1] 1169401
```

The central mortality rate and the death probability are linked by the
usual conversions:

``` r
m <- mxt(soa08Act, 65, 1)
c(mx2qx(m), qxt(soa08Act, 65, 1))
#> [1] 0.02132028 0.02132028
```

## Expectation of life: curtate and complete

`exn` returns the expectation of future lifetime. With
`type = "curtate"` (the default) it is the expected number of *complete*
future years, $e_{x} = \sum_{k \geq 1}{}_{k}p_{x}$; with
`type = "complete"` it is the expected *exact* future lifetime,
${\mathring{e}}_{x} = T_{x}/l_{x}$, evaluated under the uniform
distribution of deaths (UDD) within each year of age. Under UDD the two
are related by ${\mathring{e}}_{x} \approx e_{x} + 0.5$:

``` r
curtate  <- exn(soa08Act, x = 0, type = "curtate")
complete <- exn(soa08Act, x = 0, type = "complete")
c(curtate = curtate, complete = complete, difference = complete - curtate)
#>    curtate   complete difference 
#>   71.30789   71.80789    0.50000
```

Both forms accept a temporary horizon `n`; the temporary complete
expectation is ${\mathring{e}}_{x:\overline{n}|} = {}_{n}L_{x}/l_{x}$:

``` r
exn(soa08Act, x = 50, n = 20, type = "curtate")
#> [1] 17.86296
exn(soa08Act, x = 50, n = 20, type = "complete")
#> [1] 17.99338
```

As a published check, Finan’s *Exam MLC* study manual ([Finan
2014](#ref-finanMLC)) (Example 23.24) uses the small extract below and
obtains $e_{80} = 2.3$:

``` r
t80 <- new("lifetable", x = 80:86,
           lx = c(250, 217, 161, 107, 62, 28, 0), name = "Finan 23.24")
exn(t80, 80)
#> [1] 2.3
```

## Within-year deaths and the open last interval

`Tx` and `exn` (complete branch) expose two assumptions that matter when
a life table is to be matched against an officially published one:

- `fxt` — the fraction of the final year of age lived by those who die
  within it, so that $L_{x} = l_{x} - fxt\, d_{x}$. The default `0.5` is
  the UDD assumption.
- `axOmega` — the mean number of years lived in the last, *open-ended*
  age interval by those still alive at the last tabulated age $\omega$,
  so that $L_{\omega} = axOmega \cdot l_{\omega}$. The default `1 - fxt`
  reproduces the historical closed-interval behaviour of the package
  (the last age contributes $0.5\, l_{\omega}$ when `fxt = 0.5`).

Official period life tables (for example the U.S. NCHS tables) leave the
last interval open and close it with
$L_{\omega} = l_{\omega}/m_{\omega}$, i.e. `axOmega = 1 / mOmega`. The
excerpt below reproduces the published closure of the last age of the
NCHS 2019 total table ($l_{100} = 2090$, $m_{100}$ implied by
$L_{100} = 4696$):

``` r
tail2 <- new("lifetable", x = c(99, 100), lx = c(3024, 2090), name = "NCHS tail")
mOmega <- 2090 / 4696                 # published central rate at the open age
c(default_closed = Tx(tail2, 100),
  open_interval  = Tx(tail2, 100, axOmega = 1 / mOmega),
  published_L100 = 4696)
#> default_closed  open_interval published_L100 
#>           1045           4696           4696
```

With the defaults the results are identical to previous versions of the
package, so existing code is unaffected.

## Dispersion of the future lifetime

`varxn` and `sdxn` are the second-moment companions of `exn`: the
variance and standard deviation of the future lifetime. For the curtate
lifetime the variance is computed exactly from the table; for the
complete lifetime it uses the UDD relation
${Var}(T_{x}) = {Var}(K_{x}) + 1/12$.

``` r
varxn(soa08Act, x = 65, type = "Kx")        # Var(K_65)
#> [1] 68.34241
sdxn(soa08Act,  x = 65, type = "Kx")        # sd(K_65)
#> [1] 8.266947
varxn(soa08Act, x = 65, type = "complete")  # Var(T_65) = Var(K_65) + 1/12
#> [1] 68.42574
```

On the Finan extract the variance of the curtate lifetime is $2.394$:

``` r
varxn(t80, 80, type = "Kx")
#> [1] 2.394
```

A temporary horizon is available for the curtate lifetime:

``` r
varxn(soa08Act, x = 40, n = 20, type = "Kx")
#> [1] 10.61487
```

## Distribution of the age at death

The age at death implied by a life table has a full distribution, not
only a mean. The package exposes it through the familiar **R** generics
`median` and `quantile`, plus the Lexis modal age at death `modalAge`.

``` r
median(soa08Act)                                   # median age at death
#> [1] 76.40541
quantile(soa08Act, probs = c(0.1, 0.25, 0.75, 0.9))
#>      10%      25%      75%      90% 
#> 49.00112 65.21144 84.53137 90.29272
modalAge(soa08Act, startAge = 10)                  # adult modal age at death
#> [1] 81
modalAge(soa08Act, startAge = 10, interpolate = TRUE)
#> [1] 80.94644
```

Both `median` and `quantile` can be conditioned on survival to a given
age via the `age` argument, returning the distribution of the age at
death given that the life has reached `age`:

``` r
median(soa08Act, age = 65)
#> [1] 80.46888
quantile(soa08Act, probs = c(0.25, 0.75), age = 65)
#>      25%      75% 
#> 74.05067 86.65497
```

Because these are the standard generics, they behave as users expect:
the median is exactly the 50% quantile, and on a de Moivre table
($l_{x} = \omega - x$) the quantiles are exact.

``` r
dm <- new("lifetable", x = 0:100, lx = 100 - (0:100), name = "de Moivre")
c(median = median(dm), q25 = unname(quantile(dm, 0.25)),
  q75 = unname(quantile(dm, 0.75)))
#> median    q25    q75 
#>     50     25     75
```

## An enriched life-table printout

Finally, the `print` method of a `lifetable` forwards the `exType`,
`fxt` and `axOmega` arguments to the table it builds, so the complete
expectation (and any non-default within-year or open-interval
assumption) can be shown directly. (An `actuarialtable` such as
`soa08Act` prints its commutation functions instead, so we coerce it to
a plain `lifetable` here.)

``` r
ltSoa <- new("lifetable", x = soa08Act@x, lx = soa08Act@lx, name = "SOA 2008")
head(print(ltSoa, exType = "complete"))
#> Life table SOA 2008 
#> 
#>       x           lx           px           Lx           Tx         ex
#> 1     0 1.000000e+05 9.795783e-01 9.897892e+04 7.180789e+06 71.8078851
#> 2     1 9.795783e+04 9.986569e-01 9.789205e+04 7.081810e+06 72.2944720
#> 3     2 9.782626e+04 9.987763e-01 9.776641e+04 6.983918e+06 71.3910288
#> 4     3 9.770655e+04 9.988761e-01 9.765165e+04 6.886151e+06 70.4778845
#> 5     4 9.759674e+04 9.989579e-01 9.754589e+04 6.788499e+06 69.5566211
#> 6     5 9.749503e+04 9.990230e-01 9.744741e+04 6.690954e+06 68.6286601
#> 7     6 9.739978e+04 9.990731e-01 9.735464e+04 6.593506e+06 67.6952869
#> 8     7 9.730950e+04 9.991096e-01 9.726618e+04 6.496152e+06 66.7576280
#> 9     8 9.722286e+04 9.991340e-01 9.718076e+04 6.398885e+06 65.8166764
#> 10    9 9.713866e+04 9.991478e-01 9.709727e+04 6.301705e+06 64.8732897
#> 11   10 9.705588e+04 9.991525e-01 9.701475e+04 6.204607e+06 63.9281954
#> 12   11 9.697363e+04 9.991496e-01 9.693239e+04 6.107593e+06 62.9819964
#> 13   12 9.689116e+04 9.991404e-01 9.684952e+04 6.010660e+06 62.0351764
#> 14   13 9.680788e+04 9.991270e-01 9.676562e+04 5.913811e+06 61.0881153
#> 15   14 9.672336e+04 9.991102e-01 9.668033e+04 5.817045e+06 60.1410579
#> 16   15 9.663730e+04 9.990919e-01 9.659342e+04 5.720365e+06 59.1941717
#> 17   16 9.654954e+04 9.990718e-01 9.650473e+04 5.623771e+06 58.2475201
#> 18   17 9.645992e+04 9.990498e-01 9.641409e+04 5.527267e+06 57.3011708
#> 19   18 9.636826e+04 9.990256e-01 9.632131e+04 5.430852e+06 56.3551963
#> 20   19 9.627436e+04 9.989991e-01 9.622618e+04 5.334531e+06 55.4096743
#> 21   20 9.617800e+04 9.989701e-01 9.612848e+04 5.238305e+06 54.4646876
#> 22   21 9.607895e+04 9.989382e-01 9.602794e+04 5.142177e+06 53.5203249
#> 23   22 9.597693e+04 9.989033e-01 9.592430e+04 5.046149e+06 52.5766808
#> 24   23 9.587167e+04 9.988650e-01 9.581727e+04 4.950224e+06 51.6338561
#> 25   24 9.576286e+04 9.988230e-01 9.570651e+04 4.854407e+06 50.6919586
#> 26   25 9.565015e+04 9.987770e-01 9.559166e+04 4.758700e+06 49.7511027
#> 27   26 9.553317e+04 9.987265e-01 9.547234e+04 4.663109e+06 48.8114106
#> 28   27 9.541151e+04 9.986712e-01 9.534812e+04 4.567636e+06 47.8730121
#> 29   28 9.528473e+04 9.986105e-01 9.521853e+04 4.472288e+06 46.9360452
#> 30   29 9.515233e+04 9.985440e-01 9.508306e+04 4.377070e+06 46.0006564
#> 31   30 9.501379e+04 9.984711e-01 9.494116e+04 4.281987e+06 45.0670014
#> 32   31 9.486853e+04 9.983911e-01 9.479221e+04 4.187046e+06 44.1352450
#> 33   32 9.471589e+04 9.983035e-01 9.463555e+04 4.092253e+06 43.2055618
#> 34   33 9.455520e+04 9.982073e-01 9.447045e+04 3.997618e+06 42.2781368
#> 35   34 9.438570e+04 9.981020e-01 9.429613e+04 3.903147e+06 41.3531652
#> 36   35 9.420655e+04 9.979864e-01 9.411171e+04 3.808851e+06 40.4308535
#> 37   36 9.401686e+04 9.978598e-01 9.391625e+04 3.714740e+06 39.5114192
#> 38   37 9.381564e+04 9.977209e-01 9.370873e+04 3.620823e+06 38.5950918
#> 39   38 9.360183e+04 9.975687e-01 9.348804e+04 3.527115e+06 37.6821125
#> 40   39 9.337425e+04 9.974018e-01 9.325295e+04 3.433627e+06 36.7727351
#> 41   40 9.313164e+04 9.972188e-01 9.300213e+04 3.340374e+06 35.8672258
#> 42   41 9.287262e+04 9.970182e-01 9.273416e+04 3.247371e+06 34.9658638
#> 43   42 9.259570e+04 9.967983e-01 9.244746e+04 3.154637e+06 34.0689412
#> 44   43 9.229923e+04 9.965573e-01 9.214035e+04 3.062190e+06 33.1767637
#> 45   44 9.198147e+04 9.962930e-01 9.181098e+04 2.970049e+06 32.2896498
#> 46   45 9.164050e+04 9.960034e-01 9.145737e+04 2.878239e+06 31.4079317
#> 47   46 9.127425e+04 9.956859e-01 9.107736e+04 2.786781e+06 30.5319548
#> 48   47 9.088048e+04 9.953379e-01 9.066863e+04 2.695704e+06 29.6620779
#> 49   48 9.045678e+04 9.949564e-01 9.022867e+04 2.605035e+06 28.7986725
#> 50   49 9.000055e+04 9.945383e-01 8.975477e+04 2.514806e+06 27.9421231
#> 51   50 8.950900e+04 9.940801e-01 8.924405e+04 2.425052e+06 27.0928263
#> 52   51 8.897911e+04 9.935779e-01 8.869340e+04 2.335808e+06 26.2511907
#> 53   52 8.840768e+04 9.930276e-01 8.809947e+04 2.247114e+06 25.4176360
#> 54   53 8.779126e+04 9.924245e-01 8.745873e+04 2.159015e+06 24.5925924
#> 55   54 8.712620e+04 9.917636e-01 8.676740e+04 2.071556e+06 23.7764995
#> 56   55 8.640860e+04 9.910395e-01 8.602146e+04 1.984789e+06 22.9698056
#> 57   56 8.563433e+04 9.902462e-01 8.521670e+04 1.898767e+06 22.1729663
#> 58   57 8.479907e+04 9.893770e-01 8.434866e+04 1.813550e+06 21.3864432
#> 59   58 8.389825e+04 9.884248e-01 8.341268e+04 1.729202e+06 20.6107024
#> 60   59 8.292711e+04 9.873819e-01 8.240392e+04 1.645789e+06 19.8462131
#> 61   60 8.188073e+04 9.862396e-01 8.131737e+04 1.563385e+06 19.0934456
#> 62   61 8.075401e+04 9.849886e-01 8.014790e+04 1.482068e+06 18.3528693
#> 63   62 7.954178e+04 9.836187e-01 7.889028e+04 1.401920e+06 17.6249510
#> 64   63 7.823878e+04 9.821188e-01 7.753928e+04 1.323030e+06 16.9101522
#> 65   64 7.683978e+04 9.804769e-01 7.608970e+04 1.245490e+06 16.2089274
#> 66   65 7.533963e+04 9.786797e-01 7.453650e+04 1.169401e+06 15.5217210
#> 67   66 7.373337e+04 9.767129e-01 7.287485e+04 1.094864e+06 14.8489652
#> 68   67 7.201633e+04 9.745609e-01 7.110032e+04 1.021989e+06 14.1910772
#> 69   68 7.018431e+04 9.722068e-01 6.920898e+04 9.508890e+05 13.5484567
#> 70   69 6.823366e+04 9.696320e-01 6.719760e+04 8.816800e+05 12.9214828
#> 71   70 6.616154e+04 9.668167e-01 6.506381e+04 8.144824e+05 12.3105121
#> 72   71 6.396608e+04 9.637392e-01 6.280635e+04 7.494186e+05 11.7158750
#> 73   72 6.164662e+04 9.603760e-01 6.042527e+04 6.866123e+05 11.1378741
#> 74   73 5.920393e+04 9.567018e-01 5.792222e+04 6.261870e+05 10.5767810
#> 75   74 5.664050e+04 9.526892e-01 5.530065e+04 5.682648e+05 10.0328342
#> 76   75 5.396080e+04 9.483089e-01 5.256615e+04 5.129641e+05  9.5062368
#> 77   76 5.117151e+04 9.435292e-01 4.972666e+04 4.603980e+05  8.9971547
#> 78   77 4.828181e+04 9.383160e-01 4.679270e+04 4.106713e+05  8.5057147
#> 79   78 4.530360e+04 9.326329e-01 4.377761e+04 3.638786e+05  8.0320029
#> 80   79 4.225162e+04 9.264411e-01 4.069763e+04 3.201010e+05  7.5760639
#> 81   80 3.914364e+04 9.196991e-01 3.757201e+04 2.794034e+05  7.1378994
#> 82   81 3.600037e+04 9.123631e-01 3.442289e+04 2.418314e+05  6.7174683
#> 83   82 3.284541e+04 9.043866e-01 3.127518e+04 2.074085e+05  6.3146861
#> 84   83 2.970495e+04 8.957206e-01 2.815614e+04 1.761333e+05  5.9294254
#> 85   84 2.660734e+04 8.863140e-01 2.509489e+04 1.479771e+05  5.5615168
#> 86   85 2.358245e+04 8.761133e-01 2.212168e+04 1.228823e+05  5.2107493
#> 87   86 2.066090e+04 8.650633e-01 1.926694e+04 1.007606e+05  4.8768725
#> 88   87 1.787299e+04 8.531074e-01 1.656028e+04 8.149363e+04  4.5595977
#> 89   88 1.524758e+04 8.401879e-01 1.402921e+04 6.493335e+04  4.2586007
#> 90   89 1.281083e+04 8.262467e-01 1.169787e+04 5.090414e+04  3.9735239
#> 91   90 1.058491e+04 8.112262e-01 9.585830e+03 3.920628e+04  3.7039792
#> 92   91 8.586754e+03 7.950702e-01 7.706913e+03 2.962044e+04  3.4495510
#> 93   92 6.827072e+03 7.777251e-01 6.068328e+03 2.191353e+04  3.2097996
#> 94   93 5.309585e+03 7.591411e-01 4.670155e+03 1.584520e+04  2.9842642
#> 95   94 4.030724e+03 7.392743e-01 3.505268e+03 1.117505e+04  2.7724668
#> 96   95 2.979811e+03 7.180878e-01 2.559788e+03 7.669782e+03  2.5739157
#> 97   96 2.139766e+03 6.955544e-01 1.814045e+03 5.109994e+03  2.3881090
#> 98   97 1.488323e+03 6.716590e-01 1.243985e+03 3.295949e+03  2.2145382
#> 99   98 9.996458e+02 6.464007e-01 8.229088e+02 2.051964e+03  2.0526915
#> 100  99 6.461717e+02 6.197959e-01 5.233331e+02 1.229056e+03  1.9020573
#> 101 100 4.004946e+02 5.918812e-01 3.187699e+02 7.057225e+02  1.7621275
#> 102 101 2.370452e+02 5.627163e-01 1.852172e+02 3.869526e+02  1.6324001
#> 103 102 1.333892e+02 5.323867e-01 1.022019e+02 2.017354e+02  1.5123821
#> 104 103 7.101463e+01 5.010065e-01 5.329671e+01 9.953351e+01  1.4015917
#> 105 104 3.557879e+01 4.687207e-01 2.612766e+01 4.623680e+01  1.2995607
#> 106 105 1.667652e+01 4.357063e-01 1.197129e+01 2.010915e+01  1.2058360
#> 107 106 7.266065e+00 4.021734e-01 5.094141e+00 8.137855e+00  1.1199811
#> 108 107 2.922218e+00 3.683640e-01 1.999329e+00 3.043714e+00  1.0415767
#> 109 108 1.076440e+00 3.345505e-01 7.182815e-01 1.044385e+00  0.9702217
#> 110 109 3.601234e-01 3.010315e-01 2.342659e-01 3.261036e-01  0.9055328
#> 111 110 1.084085e-01 2.681258e-01 6.873780e-02 9.183761e-02  0.8471442
#> 112 111 2.906711e-02 2.361648e-01 1.796587e-02 2.309981e-02  0.7947062
#> 113 112 6.864628e-03 2.054818e-01 4.137592e-03 5.133944e-03  0.7478838
#> 114 113 1.410556e-03 1.764007e-01 8.296895e-04 9.963520e-04  0.7063541
#> 115 114 2.488230e-04 1.492221e-01 1.429764e-04 1.666625e-04  0.6698034
#> 116 115 3.712990e-05 1.242099e-01 2.087090e-05 2.368604e-05  0.6379236
#> 117 116 4.611900e-06 1.015759e-01 2.540179e-06 2.815139e-06  0.6104076
#> 118 117 4.684580e-07 8.146984e-02 2.533116e-07 2.749598e-07  0.5869466
#> 119 118 3.816520e-08 6.396770e-02 2.030327e-08 2.164824e-08  0.5672247
#> 120 119 2.441340e-09 4.906732e-02 1.280565e-09 1.344974e-09  0.5509164
#> 121 120 1.197900e-10 3.668695e-02 6.209237e-11 6.440918e-11  0.5376841
#> 122 121 4.394730e-12 2.667149e-02 2.255972e-12 2.316811e-12  0.5271795
#> 123 122 1.172140e-13 1.880287e-02 5.970898e-14 6.083945e-14  0.5190459
#> 124 123 2.203960e-15 1.281602e-02 1.116103e-15 1.130465e-15  0.5129245
#> 125 124 2.824600e-17 8.418289e-03 1.424189e-17 1.436205e-17  0.5084631
#> 126 125 2.377830e-19 5.309841e-03 1.195228e-19 1.201581e-19  0.5053269
#> 127 126 1.262590e-21 3.203542e-03 6.333174e-22 6.353472e-22  0.5032094
#> 128 127 4.044760e-24 1.840806e-03 2.026103e-24 2.029833e-24  0.5018427
#> 129 128 7.445620e-27 1.002697e-03 3.726543e-27 3.730280e-27  0.5010032
#> 130 129 7.465700e-30 5.150836e-04 3.734773e-30 3.736696e-30  0.5005152
#> 131 130 3.845460e-33 2.481274e-04 1.923207e-33 1.923684e-33  0.5002482
#> 132 131 9.541640e-37 1.113959e-04 4.771351e-37 4.771883e-37  0.5001114
#> 133 132 1.062900e-40 4.629213e-05 5.314746e-41 5.314992e-41  0.5000463
#> 134 133 4.920390e-45 1.767472e-05 2.460238e-45 2.460282e-45  0.5000177
#> 135 134 8.696650e-50 6.149724e-06 4.348352e-50 4.348378e-50  0.5000061
#> 136 135 5.348200e-55 1.932519e-06 2.674105e-55 2.674110e-55  0.5000019
#> 137 136 1.033550e-60 5.431077e-07 5.167753e-61 5.167756e-61  0.5000005
#> 138 137 5.613290e-67 1.350422e-07 2.806645e-67 2.806646e-67  0.5000001
#> 139 138 7.580310e-74 2.935883e-08 3.790155e-74 3.790155e-74  0.5000000
#> 140 139 2.225490e-81 5.508989e-09 1.112745e-81 1.112745e-81  0.5000000
#>   x        lx        px       Lx      Tx       ex
#> 1 0 100000.00 0.9795783 98978.92 7180789 71.80789
#> 2 1  97957.83 0.9986569 97892.05 7081810 72.29447
#> 3 2  97826.26 0.9987763 97766.41 6983918 71.39103
#> 4 3  97706.55 0.9988761 97651.65 6886151 70.47788
#> 5 4  97596.74 0.9989579 97545.89 6788499 69.55662
#> 6 5  97495.03 0.9990230 97447.41 6690954 68.62866
```

## References

Bowers, N. L., D. A. Jones, H. U. Gerber, C. J. Nesbitt, and J. C.
Hickman. 1997. *Actuarial Mathematics, 2nd Edition*. Society of
Actuaries.

Dickson, D. C. M., M. R. Hardy, and H. R. Waters. 2009. *Actuarial
Mathematics for Life Contingent Risks*. International Series on
Actuarial Science. Cambridge University Press.

Finan, Marcel A. 2014. *A Reading of the Theory of Life Contingency
Models: A Preparation for Exam MLC*. Lecture notes.

Keyfitz, N., and H. Caswell. 2005. *Applied Mathematical Demography*.
Statistics for Biology and Health. Springer-Verlag.
