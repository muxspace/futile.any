# Get either names or colnames from a list or data.frame. This attempts to 
# create some polymorphism around lists, vectors, and data.frames.
anynames <- function(data)
{
  ns <- names(data)
  if (is.null(ns)) { ns <- colnames(data) }
  ns
}

"anynames<-" <- function(data, value)
{
  if (is.null(names(data))) colnames(data) <- value
  else names(data) <- value
  invisible(data)
}

# Gets the length of a vector or the rows of a matrix or data frame.
anylength <- function(data)
{
  len <- nrow(data)
  if (is.null(len)) { len <- length(data) }
  len
}

# Lists out the types of a data.frame or other object that supports anynames
anytypes <- function(data, fun=class)
{
  ts <- apply(matrix(anynames(data), ncol=1), 1, function(x) fun(data[,x]))
  names(ts) <- anynames(data)

  return(ts)
}
