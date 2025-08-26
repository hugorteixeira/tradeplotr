## Declare globals used in NSE contexts to silence R CMD check NOTES
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "cup", "cdown", "cline", "clw", "cw", "cgrp",
    "rs_txt", "rs_fill", "rs_stk"
  ))
}

