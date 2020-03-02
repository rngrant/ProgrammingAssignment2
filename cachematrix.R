## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function returns a set of functions supporting
#  a matrix with it's inverse cached
makeCacheMatrix <- function(matrix = matrix()) {
  cachematrixinverse <- NULL
  set <- function(newmatrix){
    matrix <<- newmatrix
    cachematrixinverse <<- NULL
  }
  get <- function() matrix
  getinverse <- function() cachematrixinverse
  setinverse <- function(newinverse){
    cachematrixinverse <<- newinverse
  } 
  list(set = set, get = get, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function
#  Returns the inverse of a cached matrix. If there
#  is already a saved inverse, return that rather than
#  recomputing it
#  This function as with the one above is based closely on the 
#  cache mean functions in the project assignment page
#  https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ret <- x$getinverse()
  if (!is.null(ret)){
    message("getting cached data")
    return(ret)
  }
  matrix <- x$get()
  ret <- solve(matrix,...)
  x$setinverse(ret)
  ret
}
