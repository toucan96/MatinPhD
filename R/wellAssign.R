wellAssign <- function(layout, data) {
  # layout and data as lists of matrices
  s <- length(data[[1]])
  f <- length(layout[[1]])
  if(s == f) {
    cat("Plate format:", f, "\n \n")
    } else {
      stop("Data format does not match layout format")
    }

  c <- 0
  n <- length(layout)
  e <- 0
  o <- length(data)
  output <- data.frame(r = seq(1,f))

  repeat {
    c <- c + 1
    dim(layout[[c]]) <- f
    output<-cbind(output, layout[c])
    if(c >= n) {
      break
    }
  }

  repeat {
    e <- e + 1
    dim(data[[e]]) <- s
    output<-cbind(output, data[e])
    if(e >= o) {
      break
    }
  }

  output <- output[-1]
  return(output)
}
