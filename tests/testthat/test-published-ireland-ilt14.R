library(testthat)
library(lifecontingencies)

context("Irish Life Table No. 14 (2001-2003): personal injury multipliers")

# Life table: Central Statistics Office (CSO), "Irish Life Table No. 14,
# 2001-2003", lx by single year of age 0-105, radix 100,000:
# https://cso.ie/en/media/csoie/releasespublications/documents/birthsdm/2003/irishlife_2001-2003.pdf
# Multipliers: Society of Actuaries in Ireland, "Principles and Practice of
# Assessing Damages for Personal Injury and Wrongful Death in Ireland"
# (2008), Appendix 1, Table 1 "Multiplier for loss of one unit per annum,
# based on ILT 14 Males and Females", at real rates 0%, 2% and 4%:
# https://web.actuaries.ie/sites/default/files/081118_principles_and_practice_of_assessing_damages_for_personal_injury_and_wrongful_death_in_ireland.pdf
#
# The printed multipliers are reproduced by an annuity paid in the middle of
# each year, with survival to the middle of the year by linear interpolation
# (UDD): in the package, axn() with a half-year deferral m = 0.5. The life
# expectancy at birth implied by the lx (75.1 men, 80.3 women) matches the
# CSO figures.
# The table stops at age 105 (l_105 = 9 men, 102 women); the "for life"
# multipliers include survival beyond 105, which is not printed, so they are
# compared within 0.01 (the "to age 65" ones are exact to two decimals).

ilt14 <- read.csv(text = "
x,lx_male,lx_female
0,100000,100000
1,99349,99484
2,99301,99447
3,99261,99424
4,99240,99406
5,99220,99390
6,99205,99380
7,99194,99372
8,99179,99362
9,99167,99352
10,99157,99342
11,99147,99332
12,99136,99321
13,99120,99308
14,99097,99292
15,99063,99274
16,99016,99251
17,98956,99225
18,98884,99196
19,98804,99165
20,98714,99133
21,98616,99100
22,98511,99066
23,98402,99032
24,98289,98998
25,98176,98965
26,98064,98932
27,97954,98901
28,97844,98869
29,97735,98836
30,97629,98803
31,97524,98769
32,97420,98733
33,97317,98693
34,97211,98647
35,97103,98593
36,96991,98531
37,96875,98461
38,96752,98382
39,96621,98298
40,96481,98208
41,96330,98113
42,96168,98012
43,95992,97903
44,95803,97785
45,95599,97655
46,95379,97514
47,95140,97359
48,94877,97186
49,94584,96993
50,94259,96775
51,93896,96531
52,93494,96260
53,93048,95963
54,92558,95642
55,92022,95299
56,91436,94933
57,90793,94540
58,90083,94111
59,89300,93636
60,88437,93111
61,87487,92529
62,86441,91887
63,85289,91179
64,84024,90406
65,82640,89569
66,81129,88662
67,79482,87673
68,77690,86585
69,75746,85383
70,73649,84056
71,71394,82594
72,68970,80982
73,66368,79204
74,63579,77255
75,60606,75130
76,57450,72819
77,54120,70303
78,50629,67556
79,46989,64549
80,43219,61267
81,39357,57715
82,35452,53918
83,31566,49921
84,27756,45772
85,24073,41514
86,20572,37209
87,17302,32926
88,14308,28741
89,11618,24722
90,9253,20933
91,7218,17427
92,5507,14249
93,4104,11429
94,2982,8980
95,2109,6902
96,1449,5183
97,966,3796
98,623,2708
99,388,1877
100,233,1263
101,134,823
102,74,518
103,39,314
104,20,183
105,9,102
")

ilt14_male <- new("actuarialtable", x = ilt14$x, lx = ilt14$lx_male, interest = 0.02,
                  name = "ILT 14 males")
ilt14_female <- new("actuarialtable", x = ilt14$x, lx = ilt14$lx_female, interest = 0.02,
                    name = "ILT 14 females")

# SAI (2008) Appendix 1, Table 1: age, rate, annuity to 65, annuity for life
sai_table1 <- read.csv(text = "
sex,age,rate,to65,life
M,25,0.00,38.32,51.25
M,25,0.02,26.69,31.57
M,25,0.04,19.64,21.55
M,45,0.00,19.06,32.33
M,45,0.02,15.80,23.25
M,45,0.04,13.32,17.61
M,65,0.00,NA,15.36
M,65,0.02,NA,12.81
M,65,0.04,NA,10.89
M,85,0.00,NA,4.61
M,85,0.02,NA,4.29
M,85,0.04,NA,4.02
F,25,0.00,39.06,56.01
F,25,0.02,27.11,33.34
F,25,0.04,19.89,22.27
F,45,0.00,19.41,36.59
F,45,0.02,16.07,25.45
F,45,0.04,13.52,18.80
F,65,0.00,NA,18.74
F,65,0.02,NA,15.19
F,65,0.04,NA,12.62
F,85,0.00,NA,5.81
F,85,0.02,NA,5.34
F,85,0.04,NA,4.93
")

multiplier <- function(sex, age, rate, to65) {
  tab <- if (sex == "M") ilt14_male else ilt14_female
  if (to65) axn(tab, x = age, n = 65 - age, m = 0.5, i = rate)
  else axn(tab, x = age, m = 0.5, i = rate)
}

test_that("ILT 14: life expectancy at birth matches CSO (75.1 men, 80.3 women)", {
  expect_equal(round(exn(ilt14_male, x = 0, type = "complete"), 1), 75.1)
  expect_equal(round(exn(ilt14_female, x = 0, type = "complete"), 1), 80.3)
})

test_that("ILT 14: multipliers to age 65 reproduce SAI Table 1 to two decimals", {
  rows <- sai_table1[!is.na(sai_table1$to65), ]
  calc <- unname(mapply(multiplier, rows$sex, rows$age, rows$rate, TRUE))
  expect_equal(round(calc, 2), rows$to65)
})

test_that("ILT 14: whole of life multipliers agree with SAI Table 1 within 0.01", {
  calc <- unname(mapply(multiplier, sai_table1$sex, sai_table1$age, sai_table1$rate, FALSE))
  expect_lte(max(abs(calc - sai_table1$life)), 0.01)
})
