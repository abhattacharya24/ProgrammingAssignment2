## The function makeCacheMatrix() returns a list of functions to get and set the matrix
## and get and set the value of calculated inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #get and set the inverse of matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The function cacheSolve() calculates the inverse of a matrix but first checks
## whether the inverse of the matrix is already available in the cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
