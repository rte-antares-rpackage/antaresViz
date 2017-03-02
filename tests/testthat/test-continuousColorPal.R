context("Continuous Color Scale")

describe("continuousColorPal()", {
  # Default colors
  posCol <- "#0000FF"
  zeroCol <- "#FFFFFF"
  negCol <- "#FF0000"
  # Custom colors
  custZeroCol <- "#FF0000"
  custPosCol <- "#00FF00"
  
  it ("returns a vector of colors with break points and color palette", {
    cols <- continuousColorPal(1:100)
    expect_is(cols, "character")
    expect_true(all(grepl("^#[0-9A-F]{6}", cols)))
    expect_false(is.null(attr(cols, "breaks")))
    expect_false(is.null(attr(cols, "pal")))
  })
  
  it ("works with positive values", {
    cols <- continuousColorPal(1:100)
    expect_equal(cols[1], zeroCol)
    expect_equal(cols[100], posCol)
  })
  
  it ("works with negative values", {
    cols <- continuousColorPal(-1:-100)
    expect_equal(cols[1], zeroCol)
    expect_equal(cols[100], negCol)
  })
  
  it ("works with positive and negative values", {
    cols <- continuousColorPal(-100:100)
    expect_equal(cols[1], negCol)
    expect_equal(cols[101], zeroCol)
    expect_equal(cols[201], posCol)
  })
  
  it ("modifies the number of break points", {
    cols1 <- continuousColorPal(1:100, 3)
    cols2 <- continuousColorPal(1:100, 10)
    expect_gt(length(unique(cols2)), length(unique(cols1)))
  })
  
  it ("accepts custom domains", {
    cols <- continuousColorPal(1:100, domain = c(0, 1000))
    expect_equal(max(attr(cols, "breaks")), 1000)
    expect_false(cols[100] == posCol)
  })
  
  it ("accepts custom break points", {
    cols <- continuousColorPal(1:100, breaks = c(0, 80, 100))
    expect_true(all(cols[1:80] == zeroCol))
    expect_true(all(cols[81:100] == posCol))
  })
  
  it ("accepts custom colors if custom break points", {
    cols <- continuousColorPal(1:100, breaks = c(0, 80, 100), 
                               colors = c(custZeroCol, custPosCol))
    expect_true(all(cols[1:80] == custZeroCol))
    expect_true(all(cols[81:100] == custPosCol))
  })
  
  it ("ignores custom colors if automatic break points", {
    cols <- continuousColorPal(1:100, colors = c(custZeroCol, custPosCol))
    expect_true(all(cols[1] == zeroCol))
    expect_true(all(cols[100] == posCol))
  })
})
