## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  set <- function(matrix){
    x <<- matrix
    temp <<- NULL }
  get <- function() x
  setinv <- function(inv) temp <<- inv
  getinv <- function() temp
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message ("CASH AVAILABLE")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
