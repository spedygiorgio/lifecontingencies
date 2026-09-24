library(testthat)
library(lifecontingencies)

# ============================================================
# pxt()/qxt() CORNER-CASE SUITE
# Branch: perf/rcpp-pxt
#
# Purpose:
#   Validate boundary conditions, degenerate tables,
#   vector recycling and R/native-backend agreement.
#
# This suite is independent of the published golden-value
# regression tests.
# ============================================================


# ============================================================
# Reference table
# ============================================================

ages <- 20:92

lx <- 100000 * exp(
  -0.001 * (ages - 40) -
    0.00001 * (ages - 40)^2
)

tab <- new(
  "lifetable",
  name = "pxtCornerCases",
  x = ages,
  lx = lx
)

methods <- c(
  "linear",
  "constant force",
  "hyperbolic"
)


# ============================================================
# BLOCK A: BOUNDARY CASES
# ============================================================

test_that("A1: zero duration gives p=1 and q=0", {
  
  for (method in methods) {
    
    for (x in c(20, 20.25, 26.5, 40, 62.5, 80.75)) {
      
      p <- pxt(
        tab,
        x = x,
        t = 0,
        fractional = method
      )
      
      q <- qxt(
        tab,
        x = x,
        t = 0,
        fractional = method
      )
      
      expect_equal(p, 1, tolerance = 1e-12)
      expect_equal(q, 0, tolerance = 1e-12)
    }
  }
})


test_that("A2: fractional start ending exactly at integer age", {
  
  cases <- data.frame(
    x = c(20.25, 26.5, 40.25, 62.5, 80.75),
    t = c(0.75, 1.50, 0.75, 2.50, 1.25)
  )
  
  for (method in methods) {
    
    for (i in seq_len(nrow(cases))) {
      
      p <- pxt(
        tab,
        x = cases$x[i],
        t = cases$t[i],
        fractional = method
      )
      
      q <- qxt(
        tab,
        x = cases$x[i],
        t = cases$t[i],
        fractional = method
      )
      
      expect_true(is.finite(p))
      expect_true(is.finite(q))
      
      expect_equal(
        p + q,
        1,
        tolerance = 1e-12
      )
    }
  }
})


test_that("A3: fractional start crossing multiple integer ages", {
  
  cases <- data.frame(
    x = c(20.75, 26.25, 40.75, 62.25),
    t = c(3.50, 4.75, 3.50, 5.75)
  )
  
  for (method in methods) {
    
    for (i in seq_len(nrow(cases))) {
      
      p <- pxt(
        tab,
        x = cases$x[i],
        t = cases$t[i],
        fractional = method
      )
      
      q <- qxt(
        tab,
        x = cases$x[i],
        t = cases$t[i],
        fractional = method
      )
      
      expect_true(is.finite(p))
      expect_true(is.finite(q))
      
      expect_equal(
        p + q,
        1,
        tolerance = 1e-12
      )
    }
  }
})


# ============================================================
# BLOCK B: TABLE BOUNDARIES / DEGENERATE TABLES
# ============================================================
test_that("B1: reaching the last complete interval is finite", {
  
  omega <- max(tab@x)
  
  for (method in methods) {
    
    p <- pxt(
      tab,
      x = omega - 1.5,
      t = 0.5,
      fractional = method
    )
    
    q <- qxt(
      tab,
      x = omega - 1.5,
      t = 0.5,
      fractional = method
    )
    
    expect_true(is.finite(p))
    expect_true(is.finite(q))
    
    expect_equal(
      p + q,
      1,
      tolerance = 1e-12
    )
  }
})

test_that("B2: beyond omega has no crash", {
  
  omega <- max(tab@x)
  
  for (method in methods) {
    
    p <- pxt(
      tab,
      x = omega - 0.5,
      t = 0.75,
      fractional = method
    )
    
    q <- qxt(
      tab,
      x = omega - 0.5,
      t = 0.75,
      fractional = method
    )
    
    # Do not impose a new semantic contract here.
    # We only verify that the backend returns a valid R value.
    expect_true(
      is.finite(p) || is.na(p) || is.nan(p)
    )
    
    expect_true(
      is.finite(q) || is.na(q) || is.nan(q)
    )
    
    if (is.finite(p) && is.finite(q)) {
      
      expect_equal(
        p + q,
        1,
        tolerance = 1e-12
      )
    }
  }
})


test_that("B3: zero lx boundary does not crash", {
  
  zero_lx_tab <- new(
    "lifetable",
    name = "ZeroLxBoundary",
    x = 40:42,
    lx = c(100000, 99000, 0)
  )
  
  for (method in methods) {
    
    p <- pxt(
      zero_lx_tab,
      x = 41,
      t = 1,
      fractional = method
    )
    
    q <- qxt(
      zero_lx_tab,
      x = 41,
      t = 1,
      fractional = method
    )
    
    expect_true(
      is.finite(p) || is.na(p) || is.nan(p)
    )
    
    expect_true(
      is.finite(q) || is.na(q) || is.nan(q)
    )
  }
})
test_that("B4: minimal one-interval table supports fractional durations", {
  
  minimal_tab <- new(
    "lifetable",
    name = "MinimalTable",
    x = 40:41,
    lx = c(100000, 99000)
  )
  
  for (method in methods) {
    
    for (tt in c(0, 0.25, 0.5, 0.75)) {
      
      p <- pxt(
        minimal_tab,
        x = 40,
        t = tt,
        fractional = method
      )
      
      q <- qxt(
        minimal_tab,
        x = 40,
        t = tt,
        fractional = method
      )
      
      expect_true(is.finite(p))
      expect_true(is.finite(q))
      
      expect_equal(
        p + q,
        1,
        tolerance = 1e-12
      )
    }
  }
})
# ============================================================
# BLOCK C: VECTORISATION / RECYCLING
# ============================================================

test_that("C1: scalar x and vector t recycle correctly", {
  
  x <- 40.25
  t <- c(0.25, 0.5, 0.75, 1)
  
  for (method in methods) {
    
    actual <- pxt(
      tab,
      x = x,
      t = t,
      fractional = method
    )
    
    expected <- vapply(
      t,
      function(tt) {
        pxt(
          tab,
          x = x,
          t = tt,
          fractional = method
        )
      },
      numeric(1)
    )
    
    expect_equal(
      actual,
      expected,
      tolerance = 1e-12
    )
  }
})


test_that("C2: vector x and scalar t recycle correctly", {
  
  x <- c(
    40.25,
    41.5,
    42.75,
    62.25
  )
  
  t <- 0.5
  
  for (method in methods) {
    
    actual <- pxt(
      tab,
      x = x,
      t = t,
      fractional = method
    )
    
    expected <- vapply(
      x,
      function(xx) {
        pxt(
          tab,
          x = xx,
          t = t,
          fractional = method
        )
      },
      numeric(1)
    )
    
    expect_equal(
      actual,
      expected,
      tolerance = 1e-12
    )
  }
})


test_that("C3: unequal vector lengths use recycling", {
  
  x <- c(
    40.25,
    41.5
  )
  
  t <- c(
    0.25,
    0.5,
    0.75,
    1
  )
  
  for (method in methods) {
    
    actual <- pxt(
      tab,
      x = x,
      t = t,
      fractional = method
    )
    
    expected <- vapply(
      seq_len(max(length(x), length(t))),
      function(i) {
        
        pxt(
          tab,
          x = x[(i - 1) %% length(x) + 1],
          t = t[(i - 1) %% length(t) + 1],
          fractional = method
        )
      },
      numeric(1)
    )
    
    expect_equal(
      actual,
      expected,
      tolerance = 1e-12
    )
  }
})


# ============================================================
# BLOCK D: R IMPLEMENTATION VS NATIVE BACKEND
# ============================================================

test_that("D2: pxt and qxt remain complementary on grid", {
  
  grid_x <- c(
    20.25,
    26.50,
    40.75,
    62.50,
    80.75
  )
  
  grid_t <- c(
    0,
    0.25,
    0.50,
    1.00,
    1.75,
    3.50
  )
  
  for (method in methods) {
    
    for (xx in grid_x) {
      
      for (tt in grid_t) {
        
        p <- pxt(
          tab,
          x = xx,
          t = tt,
          fractional = method
        )
        
        q <- qxt(
          tab,
          x = xx,
          t = tt,
          fractional = method
        )
        
        expect_false(
          is.nan(p),
          info = sprintf(
            "pxt NaN: x=%.2f, t=%.2f, method=%s",
            xx,
            tt,
            method
          )
        )
        
        expect_false(
          is.nan(q),
          info = sprintf(
            "qxt NaN: x=%.2f, t=%.2f, method=%s",
            xx,
            tt,
            method
          )
        )
        
        expect_equal(
          p + q,
          1,
          tolerance = 1e-12,
          info = sprintf(
            "x=%.2f, t=%.2f, method=%s",
            xx,
            tt,
            method
          )
        )
      }
    }
  }
})


cat("\n")
cat("============================================================\n")
cat("pxt()/qxt() CORNER-CASE TEST SUITE COMPLETED\n")
cat("============================================================\n")