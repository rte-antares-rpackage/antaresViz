

observe({
  if(input$write_h5 > 0){
    isolate({
          print(readDirectoryInput(session, 'output_h5'))
          # Write h5
          withCallingHandlers({
            tryCatch({
              writeAntaresH5(
                path = readDirectoryInput(session, 'output_h5'),  timeSteps = input$timeSteps_h5,
                writeMcAll = input$writeMcAll_h5, misc = input$misc_h5,
                thermalAvailabilities = input$thermalAvailabilities_h5,
                hydroStorage = input$hydroStorage_h5,
                hydroStorageMaxPower = input$hydroStorageMaxPower_h5,
                reserve = input$reserve_h5,
                linkCapacity = input$linkCapacity_h5,
                mustRun = input$mustRun_h5,
                thermalModulation = input$thermalModulation_h5,
                overwrite = input$overwrite_h5,
                          opts = opts()
                        )},
              error = function(e){
                showModal(modalDialog(
                  title = "Error Writing h5",
                  easyClose = TRUE,
                  footer = NULL,
                  paste("Please update input. Error : ", e, sep = "\n")
                ))
                list()
              })}, 
            warning = function(w){
              showModal(modalDialog(
                title = "Warning Writing h5",
                easyClose = TRUE,
                footer = NULL,
                w
              ))
            }
          )
    })
  }
})


# observe directory 
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$output_h5
  },
  handlerExpr = {
    if (input$output_h5 > 0) {
      # condition prevents handler execution on initial app launch
      path = choose.dir(default = readDirectoryInput(session, 'output_h5'))
      updateDirectoryInput(session, 'output_h5', value = path)
    }
  }
)

          