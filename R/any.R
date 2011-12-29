# Get either names or colnames from a list or data.frame. This attempts to 
# create some polymorphism around lists, vectors, and data.frames.
anynames %when% (! is.null(names(data)))
anynames %as% function(data) names(data)

anynames %when% (! is.null(colnames(data)))
anynames %as% function(data) colnames(data)

"anynames<-" <- function(data, value)
{
  if (is.null(names(data))) colnames(data) <- value
  else names(data) <- value
  invisible(data)
}

# Gets the length of a vector or the rows of a matrix or data frame.
anylength %when% (! is.null(nrow(data)))
anylength %as% function(data) nrow(data)

anylength %when% (TRUE)
anylength %as% function(data) length(data)

# Lists out the types of a data.frame or other object that supports anynames
anytypes <- function(data, fun=class)
{
  ts <- apply(matrix(anynames(data), ncol=1), 1, function(x) fun(data[,x]))
  names(ts) <- anynames(data)

  return(ts)
}
