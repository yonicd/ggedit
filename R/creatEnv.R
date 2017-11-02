.creatEnv <- function() {
  .ggeditEnv <- new.env()

  assign(".ggeditEnv", envir = parent.frame(), value = .ggeditEnv)

  return(.ggeditEnv)
}

.creatEnv()
