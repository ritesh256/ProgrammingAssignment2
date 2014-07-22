## This file contain two functions to calculate Inverse of Matrix and this 
## also cache the computation of inverse of matrix if it is already computed
## before
## Written By: Ritesh, V1.0 - Initial Version

## This function creates special "matrix" which is a list of funcation to 
## set and get the value of matrix
## set and get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## This function calculates the inverse of matrix using special matrix
## created by makeCacheMatrix function.
## It first checks if inverse of matrix is already computed. If so
## then it will return the matrix directly instead recomputing.
## otherwise it will calcualted the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}

