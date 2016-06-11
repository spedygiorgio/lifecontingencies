library(lifecontingencies)

context("Actuarial Mathematics")
data(soa08Act)

test_that("Life Insurance", {
  expect_equal(round(Axn(soa08Act,x=30,n=10,i=0.04),6), 0.015773) #BOWERS P 111
  expect_equal(round(Axn(soa08Act,x=30,n=10,i=0.04,power=2)-Axn(soa08Act,x=30,n=10,i=0.04,power=1)^2,6), 0.012471) #BOWERS P 112
  expect_equal(round(1000*Axn(soa08Act,x=30,i=0.06),4), 102.4835) #BOWERS P 112
  expect_equal(round(Axn(soa08Act,x=30,i=0.06,k=4),4), 0.1048) #BOWERS P 339
  expect_equal(round(IAxn(soa08Act,x=50,i=.06),5), 4.99676) #
})

test_that("Annuities", {
  expect_equal(round(Axn(soa08Act,36)/axn(soa08Act,36),5), 0.00881) #FINAN P 437
  expect_equal(round(axn(soa08Act,x=60,k=12),5), 10.68036) #FINAN P 437
  expect_equal(round(axn(soa08Act, x=65,i=0.06),5),9.89693)
})

test_that("Single life annuities are calculated correctly", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("actuarialtable", x = x, lx = lx, interest = 0.04, name = "Linear table")
  v <- (1 + 0.04)^(-1)

  ans <- sum((lx[6:10] * v^(0:4))/lx[6])
  expect_equal(axn(tbl, x = 5), ans)

  ans <- sum((lx[6:8] * v^(0:2))/lx[6])
  expect_equal(axn(tbl, x = 5, n = 3), ans)
})

test_that("axn and axyzn return equal values for single mortality table", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("actuarialtable", x = x, lx = lx, interest = 0.04, name = "Linear table")

  expect_equal(axn(tbl, x = 5, n = 1), axyzn(list(tbl), x = 5, n = 1))
  expect_equal(axn(tbl, x = 5, n = 2), axyzn(list(tbl), x = 5, n = 2))
  expect_equal(axn(tbl, x = 5, n = 3), axyzn(list(tbl), x = 5, n = 3))
  expect_equal(axn(tbl, x = 5, n = 4), axyzn(list(tbl), x = 5, n = 4))
  expect_equal(axn(tbl, x = 5, n = 5), axyzn(list(tbl), x = 5, n = 5))
  expect_equal(axn(tbl, x = 5, n = 6), axyzn(list(tbl), x = 5, n = 6))
  expect_equal(axn(tbl, x = 5), axyzn(list(tbl), x = 5))

  expect_equal(axn(tbl, x = 0), axyzn(list(tbl), x = 0))
  expect_equal(axn(tbl, x = 1), axyzn(list(tbl), x = 1))
  expect_equal(axn(tbl, x = 2), axyzn(list(tbl), x = 2))

  expect_equal(axn(tbl, x = 6.2, k = 4), axyzn(list(tbl), x = 6.2, k = 4))
  expect_equal(axn(tbl, x = 6.9, k = 4), axyzn(list(tbl), x = 6.9, k = 4))
  expect_equal(axn(tbl, x = 7.3, k = 2), axyzn(list(tbl), x = 7.3, k = 2))
  expect_equal(axn(tbl, x = 8.7, k = 2), axyzn(list(tbl), x = 8.7, k = 2))
})

test_that("Example from Wolfgang Abele on April 27, 2015",{
  lx <- c(1000000,999887,999774.0128,999661.0383,999548.0766,999435.1277,999322.1915,999209.2681,999096.3575,998983.4596,
          998870.5744,998757.7021,998644.8424,998531.9956,998418.1629,998285.3733,998110.6734,997876.1174,997567.7736,997183.71,
          996737.9689,996228.6358,995613.9628,994840.3707,994058.4262,993268.1497,992469.5621,991662.6844,990855.471,990031.0792,
          989176.6824,988292.3584,987369.2934,986387.8483,985339.318,984246.5767,983152.0945,982055.8799,980957.9414,979858.2876,
          978736.3499,977563.8237,976312.542,974951.5623,973468.661,971837.1275,970025.6231,968047.7409,965917.0678,963641.3672,
          961216.8455,958643.668,955929.7478,953046.6637,949941.6376,946564.5951,942872.9932,938843.154,934453.1234,929720.1184,
          924679.1759,919357.6472,913747.7269,907812.0216,901483.664,894671.152,887251.6441,879108.4485,870196.9262,860517.7258,
          850046.0856,838726.8719,826440.3619,813053.681,798373.1837,782233.2714,764518.8168,745088.571,723845.3508,700854.5747,
          675995.263,649110.9314,620341.6858,589802.2646,557634.4491,524049.7991,489315.2544,453880.0223,418178.7275,382574.5725,
          347568.2339,313487.0832,280816.7133,249924.0667,221009.1018,194164.0102,169430.2336,146821.8021,126328.415,107915.0379,
          91521.2328,77059.77976,64385.06425,53392.47346,43955.8877,35935.08105,29182.62777,23549.88451,18892.0706,15072.22617,
          11963.91111,9452.949375)
  ly <- c(1000000,999941,999882.0035,999823.0104,999764.0209,999705.0348,999646.0522,999587.0731,999528.0975,999469.1253,
          999410.1566,999351.1914,999292.2297,999233.2715,999174.3167,999115.3654,999047.4256,998962.5065,998852.6207,998742.7469,
          998632.8852,998522.0369,998411.201,998300.3773,998188.5677,998076.7706,997964.986,997853.2139,997740.4565,997622.7231,997480.0631,
          997278.5721,996982.3803,996626.4576,996241.7598,995839.2782,995416.0465,994969.1047,994496.4943,993992.2846,993442.6069,992824.6856,
          992116.8016,991333.0293,990483.4569,989563.2978,988555.9223,987451.7054,986289.4747,985103.9548,983910.0088,982698.8155,981454.7188,
          980155.2728,978780.1149,977320.7538,975775.6097,974146.0644,972425.7225,970611.1761,968710.7194,966715.1753,964589.3686,962290.7522,
          959783.9848,957015.008,953917.1504,950424.8597,946489.1503,942100.2802,937252.2321,931911.7689,926038.8609,919562.1451,912345.4214,
          904234.6706,895105.5174,884808.2235,873128.755,859913.9513,844961.7675,827963.6716,808724.2798,787098.9925,762905.9308,735997.4757,
          706364.0093,673873.384,638543.5502,600545.7392,560206.4813,518015.6506,474570.196,430760.2484,387385.7067,345199.4032,304842.4862,
          266821.0071,231476.2956,199042.3,169657.8843,143372.1096,119957.867,99383.41343,81545.48209,66281.8803,53387.13818,42627.49435,
          33755.47658,26522.3193,20688.28429,16029.96538)
  male <- new("actuarialtable", x = 0:111, lx = lx, interest = 0.04, name = "Males")
  female <- new("actuarialtable", x = 0:111, lx = ly, interest = 0.04, name = "Females")

  expect_equal(axn(male, x = 100) + axn(female, x = 102) - 
                 axyzn(list(male, female), x = c(100, 102), status = "joint"), 
               axyzn(list(male, female), x = c(100, 102), status = "last"))
})

test_that("Life insurance with uniformly decreasing population at risk", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("actuarialtable", x = x, lx = lx, interest = 0, name = "Uniformly decreasing lx")

  expect_equal(Axn(tbl, x = 0), 1)
  expect_equal(Axn(tbl, x = 1), 1)
  expect_equal(Axn(tbl, x = 2), 1)
  expect_equal(Axn(tbl, x = 3), 1)
  expect_equal(Axn(tbl, x = 4), 1)
  expect_equal(Axn(tbl, x = 5), 1)
  expect_equal(Axn(tbl, x = 6), 1)
  expect_equal(Axn(tbl, x = 7), 1)
  expect_equal(Axn(tbl, x = 8), 1)
  expect_equal(Axn(tbl, x = 9), 1)

  expect_equal(Axn(tbl, x = 7.0, k = 2), 1)
  expect_equal(Axn(tbl, x = 3.0, k = 2), 1)
  expect_equal(Axn(tbl, x = 3.5, k = 2), 1)

  pmts <- rep(1,6)
  time <- seq(0.5, 3.0, by = 0.5)
  prob <- rep(5/30, 6)
  disc <- (1.06)^(-time)
  ans  <- sum(pmts * disc * prob)
  expect_equal(Axn(tbl, x = 7, k = 2, i = 0.06), ans)
})

test_that("Whole life insurance with fractional starting age", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("actuarialtable", x = x, lx = lx, interest = 0, name = "Uniformly decreasing lx")

  expect_equal(Axn(tbl, x = 8.3, k = 2), 1)
  expect_equal(Axn(tbl, x = 8.7, k = 2), 1)

  v <- 1.06^(-seq(0.5, 2.0, by = 0.5))
  p <- c(rep(5/17,3), 2/17)
  ans <- sum(p * v)
  expect_equal(Axn(tbl, x = 8.3, k = 2, i = 0.06), ans)

  v <- 1.06^(-seq(0.5, 1.5, by = 0.5))
  p <- c(rep(5/13,2), 3/13)
  ans <- sum(p * v)
  expect_equal(Axn(tbl, x = 8.7, k = 2, i = 0.06), ans)
})

test_that("Annuities with fractional starting age and k > 1", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("actuarialtable", x = x, lx = lx, interest = 0.04, name = "Uniformly decreasing lx")

  v <- 1.04^(-seq(0, 3.75, by = 0.25))
  p <- (100 - 10 * seq(6.2, 9.95, by = 0.25))/38
  ans <- sum(v * p)/4
  expect_equal(axn(tbl, x = 6.2, k = 4), ans)

  v <- 1.04^(-seq(0, 2, by = 0.25))
  p <- (100 - 10 * seq(7.9, 9.9, by = 0.25))/21
  ans <- sum(v * p)/4
  expect_equal(axn(tbl, x = 7.9, k = 4), ans)
})

test_that("Annuities with fractional age x, k > 1, and m > 0", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("actuarialtable", x = x, lx = lx, interest = 0.04, name = "Uniformly decreasing lx")

  v <- 1.04^(-(0.6+seq(0, by = 0.25, length = 13)))
  p <- (100 - 10 * seq(6.8, 9.80, by = 0.25))/38
  ans <- sum(v * p)/4
  expect_equal(axn(tbl, x = 6.2, k = 4, m = 0.6), ans)

  v <- 1.04^(-(1.4+seq(0, 0.5, by = 0.25)))
  p <- (100 - 10 * seq(9.3, 9.8, by = 0.25))/21
  ans <- sum(v * p)/4
  expect_equal(axn(tbl, x = 7.9, k = 4, m = 1.4), ans)
})
