# Class `"mdt"`

A class to store multiple decrement tables

## Objects from the Class

Objects can be created by calls of the form
`new("mdt", name, table, bottomCompletionSurvival, ...)`. They store
absolute decrements. `bottomCompletionSurvival` (default `0.99`) is the
one-year survival ratio used to synthetically back-fill ages below the
lowest age supplied in `table`; it does not affect rows for ages
actually supplied.

## Slots

- `name`::

  The name of the table

- `table`::

  A data frame containing at least the number of decrements

## Methods

- getDecrements:

  `signature(object = "mdt")`: return the name of decrements

- getOmega:

  `signature(object = "mdt")`: maximum attainable age

- initialize:

  `signature(.Object = "mdt")`: method to initialize the class

- print:

  `signature(x = "mdt")`: tabulate absolute decrement rates

- show:

  `signature(object = "mdt")`: show rates of decrement

- coerce:

  `signature(from = "mdt", to = "markovchainList")`: coercing to
  `markovchainList` objects; available only when the optional
  markovchain package is installed

- coerce:

  `signature(from = "mdt", to = "data.frame")`: coercing to `data.frame`

- summary:

  `signature(object = "mdt")`: it returns summary information about the
  object

## References

Marcel Finan A Reading of the Theory of Life Contingency Models: A
Preparation for Exam MLC/3L

## Author

Giorgio Alfredo Spedicato

## Note

Currently only decrements storage of the class is defined.

## See also

[`lifetable`](https://spedygiorgio.github.io/lifecontingencies/reference/lifetable-class.md)

## Examples

``` r
#shows the class definition
showClass("mdt")
#> Class "mdt" [package "lifecontingencies"]
#> 
#> Slots:
#>                             
#> Name:        name      table
#> Class:  character data.frame
#create a new table
tableDecr=data.frame(d1=c(150,160,160),d2=c(50,75,85))
newMdt<-new("mdt",name="testMDT",table=tableDecr)
#> Added lx 
#> Added x to the table... 
```
