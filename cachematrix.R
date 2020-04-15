## The first function 'make CacheMatrix' permits to 
## cache the inverse of a matrix object.
## The second function 'cacheSolve' calculates the inverse of the matrix object
## obtains with the first function.

## The makeCacheMatrix takes the argument x which is a matrix
## First, i initialize the final value (inverse of matrix) m, at NULL
## Second, set is a function that set the matrix x to a new matrix y 
## and reset the inverse, m, to NULL
## get returns the matrix x 
## setinverse returns the inverse of x to m
## get inverse returns the inverse matrix m
## list with all functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve (x)
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)

}



## cacheSolve takes an arguments x which is the makeCacheMatrix function
## The if loop permits to check if the inverse has already been calculated
## if so, it gets the inverse from the cache and skips the calculation.
## If not, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse()
        m
}


