context("exchangesStack")

describe("no interactivy", {

  mydata <- readAntares(links = "all", timeStep = "daily", showProgress = FALSE)
  
  # default parameters
  default_params <- exchangesStack(mydata, interactive = FALSE)
  expect_is(default_params, "htmlwidget")
  
  # TO DO : passer les arguments
  # passer plusieurs data
  # .compare
  
  # suivant les cas : 
  # - tester les retours d'erreurs
  
})