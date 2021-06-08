.onLoad <- function(libname, pkgname) {
  addResourcePath("dartsvoice",
                  system.file("assets", package = "dartsvoice"))

  invisible()
}
