## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix that can remember it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function accepts a matrix created by "makeCacheMatrix" and
## returns the inverse.  If the inverse has been previously cached,
## it returns the previously cached value.  If not, it computes the
## inverse, caches the value and returns the newly computed inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
