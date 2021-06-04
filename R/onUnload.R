.onUnload <- function (libpath) {
  library.dynam.unload("transducer", libpath)
}
