context("catColorPal")

describe("catColorPal", {
  
  it("returns a vector of colors with attributes levels and pal", {
    x <- as.factor(c("a", "b", "c", "a"))
    res <- catColorPal(x)
    expect_true(all(res == DEFAULT_CAT_COLORS[c(1,2,3,1)]))
    expect_equal(attr(res, "levels"), levels(x))
    expect_equal(attr(res, "pal"), structure(DEFAULT_CAT_COLORS[1:3], names = levels(x)))
  })
  
  it ("accepts character vectors", {
    x <- c("a", "b", "c", "a")
    res <- catColorPal(x)
    expect_true(all(res == DEFAULT_CAT_COLORS[c(1,2,3,1)]))
    expect_equal(attr(res, "levels"), unique(x))
    expect_equal(attr(res, "pal"), structure(DEFAULT_CAT_COLORS[1:3], names = unique(x)))
  })
  
  it("accepts custom colors", {
    x <- as.factor(c("a", "b", "c", "a"))
    pal <- c("red", "green", "blue")
    res <- catColorPal(x, colors = pal)
    expect_true(all(res == pal[c(1,2,3,1)]))
    expect_equal(attr(res, "levels"), levels(x))
    expect_equal(attr(res, "pal"), structure(pal, names = levels(x)))
  })
  
  it("handles missing values", {
    x <- as.factor(c("a", "b", "c", "a", NA))
    res <- catColorPal(x)
    expect_true(all(res == c(DEFAULT_CAT_COLORS[c(1,2,3,1)], "#EEEEEE")))
  })
  
  it("can reset levels", {
    x <- as.factor(c("a", "b", "a", "c"))
    res <- catColorPal(x, levels = c("a", "b", "d"))
    expect_true(all(res == c(DEFAULT_CAT_COLORS[c(1,2,1)], "#EEEEEE")))
    expect_equal(attr(res, "levels"), c("a", "b", "d"))
    expect_equal(attr(res, "pal"), structure(DEFAULT_CAT_COLORS[1:3], names = c("a", "b", "d")))
  })
})
