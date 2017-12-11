describe("prodStack, no interactive", {
    dta <- readAntares(areas = "all")



 VV <- plotXY(dta, "NODU", "LOAD", precision = 50,
         sizeOnCount = FALSE)
 expect_true("htmlwidget" %in% class(VV))
 
 
})