


.getstructure <- function(fid, strgp){
  gid <- rhdf5::H5Gopen(fid,  strgp)
  data <- rhdf5::h5dump(gid)
  rhdf5::H5Gclose(gid)
  if(length(which(data$reCalcVar!="")) > 0)
  {
    data$reCalcVar <- data$reCalcVar[which(data$reCalcVar!="")]
    data$variable <- c(data$variable, data$reCalcVar)
    data$reCalcVar <- NULL
  }
  data
}

.tryCloseH5 <- function(){
  try(rhdf5::H5close(), silent = TRUE)
}

