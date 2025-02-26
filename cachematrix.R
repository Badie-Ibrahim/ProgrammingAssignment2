## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
