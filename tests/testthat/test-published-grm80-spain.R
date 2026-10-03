library(testthat)
library(lifecontingencies)

context("Spanish GRM-80 table at 6%: published commutation table and annuity example")

# Source: "El seguro de vida en el sistema general del seguro", teaching notes
# published by M. Casares (Spain, values in pesetas),
# https://www.mcasares.es/wp-content/uploads/2016/02/el_seguro_de_vida_en_el_sistema_general_del_seguro.pdf
# The appendix prints "TABLA DE MORTALIDAD GRM-80, TIPO DE INTERES (%): 6"
# for ages 15-116 with radix l_15 = 1,000,000 and the columns
# lx, Dx, Nx, Sx, Cx, Mx, Rx (Sx and Rx below are a subset of ages). GRM/GRF-80 were the survival tables admitted
# by the Spanish insurance supervisor (DGSFP) before PERM/F-2000.
#
# Conventions found in the printed table (verified on every row):
# - Dx = v^(x-15) lx, Nx = sum of Dx, Sx = sum of Nx;
# - Cx = v^(x-15+1/2) dx: death benefits are discounted to the middle of the
#   year of death, so Mx / Dx = (1.06)^(1/2) * A_x with A_x payable at the
#   end of the year (Axn in the package);
# - values are printed with two decimals, so table checks use an absolute
#   tolerance of half a cent on the commutation scale; ratios of printed
#   values are compared with a relative tolerance of 1e-6.

grm80 <- read.csv(text = "
age,lx,Dx,Nx,Cx,Mx
15,1000000.00,1000000.00,16882832.09,726.62,45679.65
16,999251.90,942690.47,15882832.09,691.45,44953.03
17,998497.30,888659.04,14940141.61,659.40,44261.58
18,997734.50,837717.13,14051482.57,629.82,43602.19
19,996962.20,789687.44,13213765.45,602.02,42972.37
20,996179.70,744403.42,12424078.00,578.61,42370.35
21,995382.50,701705.39,11679674.58,554.96,41791.74
22,994572.00,661447.18,10977969.20,533.82,41236.78
23,993745.60,623488.28,10316522.01,514.15,40702.96
24,992901.90,587697.11,9693033.73,495.85,40188.81
25,992039.40,553949.62,9105336.62,478.85,39692.96
26,991156.50,522128.88,8551387.00,462.90,39214.11
27,990251.80,492124.81,8029258.12,447.99,38751.21
28,989323.70,463833.56,7537133.31,433.97,38303.21
29,988370.70,437157.31,7073299.76,420.71,37869.24
30,987391.40,412003.93,6636142.44,408.24,37448.53
31,986384.10,388286.44,6224138.51,398.17,37040.29
32,985342.70,365921.22,5835852.08,392.08,36642.12
33,984255.70,344827.87,5469930.86,389.66,36250.03
34,983110.60,324930.84,5125102.98,390.40,35860.37
35,981894.50,306159.35,4800172.14,394.04,35469.98
36,980593.40,288446.85,4494012.79,400.16,35075.94
37,979192.80,271730.99,4205565.95,408.48,34675.78
38,977677.30,255953.24,3933834.95,418.65,34267.29
39,976030.90,241058.69,3677881.72,430.33,33848.65
40,974237.00,226995.89,3436823.02,443.29,33418.31
41,972278.20,213716.50,3209827.13,457.23,32975.02
42,970136.60,201175.24,2996110.63,471.89,32517.79
43,967793.70,189329.62,2794935.39,487.06,32045.90
44,965230.40,178139.77,2605605.77,502.48,31558.84
45,962427.30,167568.34,2427466.00,517.95,31056.36
46,959364.50,157580.26,2259897.66,533.30,30538.41
47,956021.70,148142.63,2102317.39,548.35,30005.11
48,952378.40,139224.60,1954174.76,562.93,29456.76
49,948413.80,130797.20,1814950.16,576.89,28893.83
50,944107.10,122833.26,1684152.96,590.09,28316.94
51,939437.50,115307.28,1561319.70,602.42,27726.85
52,934384.30,108195.33,1446012.41,613.81,27124.43
53,928926.70,101474.88,1337817.08,624.10,26510.62
54,923044.60,95124.84,1236342.20,633.26,25886.52
55,916718.10,89125.34,1141217.36,641.20,25253.26
56,909927.90,83457.72,1052092.02,647.86,24612.05
57,902655.50,78104.43,968634.30,653.20,23964.19
58,894883.30,73048.99,890529.87,657.18,23310.99
59,886594.60,68275.83,817480.88,661.12,22653.82
60,877755.90,63769.03,749205.05,666.29,21992.70
61,868313.60,59512.31,685436.02,672.55,21326.41
62,858210.60,55490.44,625923.71,679.83,20653.86
63,847385.60,51689.17,570433.26,687.96,19974.03
64,835773.80,48095.16,518744.09,696.79,19286.07
65,823307.30,44696.00,470648.94,706.16,18589.27
66,809915.20,41480.16,425952.93,715.86,17883.11
67,795524.50,38436.92,384472.77,725.71,17167.25
68,780060.60,35556.38,346035.85,735.44,16441.55
69,763449.10,32829.43,310479.48,744.79,15706.11
70,745616.90,30247.76,277650.05,753.49,14961.32
71,726494.10,27803.77,247402.29,761.21,14207.83
72,706016.30,25490.62,219598.52,767.59,13446.62
73,684127.70,23302.20,194107.90,772.29,12679.03
74,660783.90,21233.10,170805.70,773.65,11906.74
75,635995.90,19279.79,149572.60,776.19,11133.10
76,609634.30,17434.58,130292.81,688.90,10356.91
77,584833.60,15778.61,112858.22,844.75,9668.01
78,552597.20,14064.98,97079.61,756.36,8823.26
79,522002.20,12534.21,83014.63,742.54,8066.90
80,490163.90,11103.50,70480.42,724.40,7324.36
81,457239.80,9771.40,59376.92,701.73,6599.96
82,423432.40,8536.72,49605.51,674.43,5898.22
83,388990.70,7398.44,41068.79,642.54,5223.79
84,354209.00,6355.58,33670.35,606.23,4581.25
85,319423.70,5407.00,27314.77,565.87,3975.02
86,285006.10,4551.32,21907.77,521.99,3409.15
87,251352.60,3786.70,17356.45,475.30,2887.16
88,218870.40,3110.71,13569.75,426.69,2411.86
89,187961.00,2520.19,10459.04,377.15,1985.17
90,159000.70,2011.22,7938.85,327.79,1608.02
91,132320.40,1579.00,5927.63,279.73,1280.23
92,108186.10,1217.92,4348.63,234.04,1000.50
93,86782.30,921.67,3130.71,191.68,766.46
94,68200.10,683.32,2209.04,153.45,574.78
95,52432.30,495.60,1525.73,119.87,421.33
96,39375.80,351.12,1030.13,91.23,301.47
97,28842.50,242.63,679.01,67.54,210.24
98,20576.40,163.30,436.38,48.56,142.70
99,14276.30,106.89,273.08,33.86,94.13
100,9619.80,67.95,166.19,22.86,60.27
101,6287.10,41.89,98.25,14.93,37.41
102,3980.50,25.02,56.35,9.41,22.48
103,2438.60,14.46,31.33,5.73,13.06
104,1444.20,8.08,16.87,3.36,7.34
105,826.10,4.36,8.79,1.90,3.98
106,456.10,2.27,4.43,1.03,2.08
107,243.00,1.14,2.16,0.54,1.05
108,124.90,0.55,1.01,0.27,0.51
109,61.90,0.26,0.46,0.13,0.24
110,29.60,0.12,0.20,0.06,0.11
111,13.60,0.05,0.09,0.03,0.05
112,6.10,0.02,0.04,0.01,0.02
113,2.60,0.01,0.01,0.00,0.01
114,1.10,0.00,0.01,0.00,0.00
115,0.40,0.00,0.00,0.00,0.00
116,0.20,0.00,0.00,0.00,0.00
")

grm80_SR <- read.csv(text = "
age,Sx,Rx
15,265641496.85,1901109.67
20,190670443.05,1679640.86
25,135579165.52,1473350.21
30,95282750.70,1279519.50
35,65991583.83,1096278.15
40,44880116.28,922940.52
45,29836814.32,760424.66
50,19278008.35,610474.18
55,12012363.99,474908.83
60,7142409.56,355114.52
65,3992667.43,251881.45
70,2055077.46,166094.16
75,945513.00,98892.61
80,372695.13,50844.42
85,118493.14,21216.84
90,27885.37,6548.46
95,4330.50,1318.46
100,386.18,148.60
")

grm80_act <- new("actuarialtable", x = grm80$age, lx = grm80$lx,
                 interest = 0.06, name = "GRM-80 (6%)")
half_year <- sqrt(1.06)
tol <- 0.005 + 1e-6
# for the cumulated columns Sx and Rx (up to 2.7e8) allow also a relative
# floating-point error of 1e-9
tol_cum <- function(ref) 0.005 + 1e-9 * abs(ref)

# D_x on the printed scale (radix 1,000,000 at age 15) through the package
D_pkg <- function(x) 1e6 * Exn(grm80_act, x = 15, n = x - 15)

test_that("GRM-80: Dx reproduced through pure endowments from age 15", {
  expect_lte(max(abs(D_pkg(grm80$age) - grm80$Dx)), tol)
})

test_that("GRM-80: Nx reproduced through whole life annuities-due", {
  N_pkg <- axn(grm80_act, x = grm80$age) * D_pkg(grm80$age)
  expect_lte(max(abs(N_pkg - grm80$Nx)), tol)
})

test_that("GRM-80: Cx and Mx reproduced through insurances (mid-year discounting)", {
  ages <- grm80$age[grm80$age < 116]
  idx <- match(ages, grm80$age)
  C_pkg <- half_year * Axn(grm80_act, x = ages, n = 1) * D_pkg(ages)
  expect_lte(max(abs(C_pkg - grm80$Cx[idx])), tol)
  M_pkg <- half_year * Axn(grm80_act, x = ages) * D_pkg(ages)
  expect_lte(max(abs(M_pkg - grm80$Mx[idx])), tol)
})

test_that("GRM-80: Sx and Rx reproduced through increasing annuities and insurances", {
  # Iaxn() and IAxn() take a single age at a time. The term is given
  # explicitly: with n missing they stop one year before getOmega(), which
  # drops the payment at age 116 where this table still has l_116 = 0.2
  # (axn() and Axn() include it).
  omega <- getOmega(grm80_act)
  D <- D_pkg(grm80_SR$age)
  S_pkg <- vapply(grm80_SR$age, function(a)
    Iaxn(grm80_act, x = a, n = omega - a + 1), numeric(1)) * D
  R_pkg <- half_year * vapply(grm80_SR$age, function(a)
    IAxn(grm80_act, x = a, n = omega - a + 1), numeric(1)) * D
  expect_true(all(abs(S_pkg - grm80_SR$Sx) <= tol_cum(grm80_SR$Sx)))
  expect_true(all(abs(R_pkg - grm80_SR$Rx) <= tol_cum(grm80_SR$Rx)))
})

test_that("GRM-80: printed example, immediate life annuity of 1,000,000 at age 60", {
  # 1.000.000 * N61 / D60 = 1.000.000 * (685.436,02 / 63.769,03) = 10.748.729 ptas.
  expect_equal(round(1e6 * axn(grm80_act, x = 60, payment = "immediate")), 10748729)
})

test_that("GRM-80: temporary and deferred contracts from commutation differences", {
  r <- function(age) grm80[grm80$age == age, ]
  # 10-year temporary annuity-due at 40: (N40 - N50) / D40
  expect_equal(axn(grm80_act, x = 40, n = 10),
               (r(40)$Nx - r(50)$Nx) / r(40)$Dx, tolerance = 1e-6)
  # deferred annuity-due from 65 for a life aged 40: N65 / D40
  expect_equal(axn(grm80_act, x = 40, m = 25),
               r(65)$Nx / r(40)$Dx, tolerance = 1e-6)
  # 20-year term insurance at 45, mid-year: (M45 - M65) / D45
  expect_equal(half_year * Axn(grm80_act, x = 45, n = 20),
               (r(45)$Mx - r(65)$Mx) / r(45)$Dx, tolerance = 1e-6)
  # 20-year endowment at 45, mid-year death benefit: (M45 - M65 + D65) / D45
  expect_equal(half_year * Axn(grm80_act, x = 45, n = 20) + Exn(grm80_act, x = 45, n = 20),
               (r(45)$Mx - r(65)$Mx + r(65)$Dx) / r(45)$Dx, tolerance = 1e-6)
  # level annual premium of the 20-year endowment, payable 20 years
  P_pkg <- (half_year * Axn(grm80_act, x = 45, n = 20) + Exn(grm80_act, x = 45, n = 20)) /
    axn(grm80_act, x = 45, n = 20)
  P_pub <- (r(45)$Mx - r(65)$Mx + r(65)$Dx) / (r(45)$Nx - r(65)$Nx)
  expect_equal(P_pkg, P_pub, tolerance = 1e-6)
})
