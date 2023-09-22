# ### AFTER ALL TESTS, CLOSE H5 FILES AND REMOVE TEMP FOLDER 
# ### DONT WRITE TEST AFTER THIS
# if (.requireRhdf5_Antares(stopP = FALSE)){
#   rhdf5::H5close()
# }
# if (dir.exists(pathtemp)){
#   unlink(pathtemp, recursive = TRUE)
# }