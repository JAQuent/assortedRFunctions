priorString_student <- function(...){
  x <- list(...)
  if(length(x) == 1){
    x <- x[[1]]
    if(class(x) == 'brmsfit'){
      if(as.character(x$family)[1] == 'student'){
        df    <- round(posterior_summary(as.data.frame(x))[3], 2)
        mu    <- round(posterior_summary(as.data.frame(x))[1], 2)
        sigma <- round(posterior_summary(as.data.frame(x))[2], 2)
      } else {
        stop("Wrong family")
        }
      } else {
        stop("Wrong length or not brmsfit")
        }
    } else {
      df    <- round(x[[1]], 2)
      mu    <- round(x[[2]], 2)
      sigma <- round(x[[3]], 2)
      }
  paste('student_t(', df, ', ', mu, ', ', sigma, ')', sep = '')
}

