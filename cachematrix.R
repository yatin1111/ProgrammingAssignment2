## Two functions that together act as creating a cahe for inverse of a matrix
## and using it to get an inverse of a matrix

## This function makes a cache of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## This function gets the inverse of a matrix for which the inverse was previosly cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getinverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}