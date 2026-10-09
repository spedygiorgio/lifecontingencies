# Function to return the decrements defined in the mdt class

This function list the character decrements of the mdf class

## Usage

``` r
getDecrements(object)
```

## Arguments

- object:

  A `mdt` class object

## Details

A character vector is returned

## Value

A character vector listing the decrements defined in the class

## References

Marcel Finan A Reading of the Theory of Life Contingency Models: A
Preparation for Exam MLC/3L

## Author

Giorgio Alfredo Spedicato

## Note

To be updated

## See also

[`getOmega`](https://spedygiorgio.github.io/lifecontingencies/reference/getOmega.md)

## Examples

``` r
#create a new table
tableDecr=data.frame(d1=c(150,160,160),d2=c(50,75,85))
newMdt<-new("mdt",name="testMDT",table=tableDecr)
#> Added lx 
#> Added x to the table... 
getDecrements(newMdt)
#> [1] "d1" "d2"
```
