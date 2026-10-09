# Plot a multiple decrement table

S4 method for the [`plot`](https://rdrr.io/r/graphics/plot.default.html)
generic: visualises the decrement structure of an `mdt` object. Three
views are available: a stacked-area chart of death counts \\d^{(j)}\_x\\
(default), a stacked bar chart, or a line chart of decrement
probabilities \\q^{(j)}\_x = d^{(j)}\_x / l^{(\tau)}\_x\\.

## Usage

``` r
# S4 method for class 'mdt,missing'
plot(x, y, type = c("area", "bar", "probability"), ...)
```

## Arguments

- x:

  An `mdt` object.

- y:

  Not used (kept for S4 generic compatibility).

- type:

  Character: one of `"area"` (default), `"bar"`, or `"probability"`.
  `"area"` and `"bar"` show stacked decrement counts; `"probability"`
  shows decrement-specific probabilities as lines.

- ...:

  Further arguments (currently unused).

## Value

A `ggplot2` object (returned invisibly).

## Examples

``` r
valdezDf <- data.frame(
  x = 50:54,
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631))
valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
#> Added fictional decrement below last x and completed x and lx until zero.... 
#> Completed the table at top, all decrements on first cause 
plot(valdezMdt)
plot(valdezMdt, type = "bar")
plot(valdezMdt, type = "probability")
```
