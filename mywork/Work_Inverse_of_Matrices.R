## This R script is a Coursera assignment to compute the inverse of cached
## matrices. 

## makeCacheMatrix: to make a function list to catch the inverse and cache it in m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() {m <<- solve(x)}
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cache Solve. argument ... may be the original matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(...)
  x$setinverse()
  m
}
## test a matrix
set.seed(13325)
mx <- matrix(rnorm(100),10,10)
cache <- makeCacheMatrix(mx)
## In this occasion there is not anything in m - (cached information)
cacheSolve(cache,mx)

## In this occasion there should be a cache in m so the function prints
## "getting cached data"
cache_2 <- makeCacheMatrix(mx)
invisible(cache_2$setinverse())
cacheSolve(cache_2,mx)