require(lifecontingencies)


#test abbreviated arguments

c(annuity(i=0.05, n=5, type = "due"), annuity(i=0.05, n=5, type = "adv"))

c(annuity(i=0.05, n=5, type = "arr"), annuity(i=0.05, n=5, type = "im"))

c(accumulatedValue(i=0.05, n=5, type = "arr"), accumulatedValue(i=0.05, n=5, type = "im"))

c(decreasingAnnuity(i=0.05, n=5, type = "arr"), decreasingAnnuity(i=0.05, n=5, type = "im"))

c(decreasingAnnuity(i=0.05, n=5, type = "due"), decreasingAnnuity(i=0.05, n=5, type = "adv"))

c(increasingAnnuity(i=0.05, n=5, type = "arr"), increasingAnnuity(i=0.05, n=5, type = "im"))

#test defensive programming 
try(
  annuity(i=0.05, n=5, type = "foo1")
)

