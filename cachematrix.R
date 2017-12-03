## This function stores the cache of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solves the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinv(m)
  m
}


## Example mean vector:
## makeVector <- function(x = numeric()) {
##   m <- NULL
##   set <- function(y) {
##     x <<- y
##     m <<- NULL
##   }
##   get <- function() x
##   setmean <- function(mean) m <<- mean
##   getmean <- function() m
##   list(set = set, get = get,
##        setmean = setmean,
##        getmean = getmean)
## }

## cachemean <- function(x, ...) {
##   m <- x$getmean()
##   if(!is.null(m)) {
##     message("getting cached data")
##     return(m)
##   }
##   data <- x$get()
##  m <- mean(data, ...)
##   x$setmean(m)
##    m
## }