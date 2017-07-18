## Functions to store "cached" values for the inverse of a matrix
## 

## Create a matrixCache object with getters and settes for matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_m <<- solve
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Get the inverse of a matrix (from cache if its executed previosly)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinv(inv_m)
  inv_m
}
