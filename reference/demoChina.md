# China Mortality Rates for life table construction

Seven yearly mortality rates for each age

## Usage

``` r
data(demoChina)
```

## Format

A data frame with 106 observations on the following 8 variables.

- `age`:

  Attained age

- `CL1`:

  CL1 rates

- `CL2`:

  CL2 rates

- `CL3`:

  CL3 rates

- `CL4`:

  CL4 rates

- `CL5`:

  CL5 rates

- `CL6`:

  CL6 rates

- `CL90-93`:

  CL 90-93 rates

## Details

See the source link for details.

## Source

Society of Actuaries

## References

<https://mort.soa.org/>

## Examples

``` r
data(demoChina)
tableChinaCL1<-probs2lifetable(probs=demoChina$CL1,radix=1000,type="qx",name="CHINA CL1")
```
