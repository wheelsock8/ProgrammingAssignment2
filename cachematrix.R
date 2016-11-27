## The makeCacheMatrix and  cacheSolve functions together compute the inverse of
## a square matrix, while providing a "caching" function that speeds up processing.
## The "caching" will store the value of the computed inverse, preventing the need to
## recalculate the inverese of the same matrix repeatedly.

## makeCacheMatrix takes in a matrix argument and creates an object that contains 
## a list of 4 functions. These functions are used to compute and then keep track of
## the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMat <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  getMat <- function() x
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  list (setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## cacheSolve utilizes functions defined within the makeCacheMatrix
## object to solve for a matrix inverse and then store it ("cache" it).
## Caching in this way can speed up operations especially for large matrices and
## repeated calculations of a matrix inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  data <- x$getMat()
  inv <- solve(data)
  x$setInv(inv)
  inv
}

