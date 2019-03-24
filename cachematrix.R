## these functions provide a means to find the inverse of a matrix and cache it, making repeated operations requiring the inverses of matrices faster provided 
## at least some of the inverses of those matrices are needed more than once

## This function returns a list containing functions to set and get an invertible matrix, x, as well as to set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() {x}
setinv <- function(inverse) {inv <<- inverse}
getinv <- function() {inv}
list(set<- set, get <- get, setinv <- setinv, getinv<- getinv)
}

## This function returns a matrix that is the inverse of x, an invertible matrix, after checking to see if the inverse has already been cached
## returning the cached value if it is not null

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
