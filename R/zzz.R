# zzz.R
#' @importFrom utils packageVersion
NULL

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  msg <- paste0(
    "\n", pkgname, " version ", version, "\n",
    "Copyright (C) 2026 Yuxuan He\n",
    "This is free software, and you are welcome to redistribute it\n",
    "under the conditions of the GNU General Public License (GPL-3).\n"
  )
  packageStartupMessage(msg)
}
