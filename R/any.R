# Get either names or colnames from a list or data.frame. This attempts to 
# create some polymorphism around lists, vectors, and data.frames.
anynames.names %when% (! is.null(names(data)))
anynames.names <- function(data) names(data)

anynames.col %when% (! is.null(colnames(data)))
anynames.col <- function(data) colnames(data)

"anynames<-" <- function(data, value)
{
  if (is.null(names(data))) colnames(data) <- value
  else names(data) <- value
  invisible(data)
}

# Gets the length of a vector or the rows of a matrix or data frame.
anylength.nrow %when% (! is.null(nrow(data)))
anylength.nrow <- function(data) nrow(data)

anylength.len %when% (TRUE)
anylength.len <- function(data) length(data)

# Lists out the types of a data.frame or other object that supports anynames
anytypes <- function(data, fun=class)
{
  ts <- apply(matrix(anynames(data), ncol=1), 1, function(x) fun(data[,x]))
  names(ts) <- anynames(data)

  return(ts)
}
