# Gets the length of a vector or the rows of a matrix or data frame.
anylength(data) %when% { ! is.null(nrow(data)) } %as% nrow(data)
anylength(data) %as% length(data)

# Get either names or colnames from a list or data.frame. This attempts to 
# create some polymorphism around lists, vectors, and data.frames.
anynames(data) %when% { ! is.null(names(data)) } %as% names(data)
anynames(data) %when% { ! is.null(colnames(data)) } %as% colnames(data)
anynames(data) %as% NULL

"anynames<-" <- function(data, value)
{
  if (is.null(names(data))) colnames(data) <- value
  else names(data) <- value
  invisible(data)
}

# Lists out the types of a data.frame or other object that supports anynames
anytypes(data, fun=class) %when% {
  is.null(dim(data))
} %as% fun(data)
anytypes(data, fun=class) %as% {
  ts <- apply(matrix(anynames(data), ncol=1), 1, function(x) fun(data[,x]))
  names(ts) <- anynames(data)
  return(ts)
}
