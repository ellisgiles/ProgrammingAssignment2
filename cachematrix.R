##  These functions create a cached matrix inversion object with helper
##  methods to get/set the inverse and values on the object.
##  The matrix inverse is cached after the first calculation so that it
##  need not be recalculated on subsequent inversion calcuation requests.

## The make cache matrix function creates a list object that has several
## embedded helper functions to set/get the value and inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##  Initialize the inverse to NULL.
  xinverse <- NULL
  ##  The set function sets the matrix and init the inverse to null.
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  ##  Get the matrix.
  get <- function() x
  ##  Set and get the inverse of the matrix.
  setinverse <- function(inv) xinverse <<- inv
  getinverse <- function() xinverse
  ##  Return a list of the functions on the matrix object.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves the inverse of a matrix object that has been created
## with the makeCacheMatrix function.
## The inverse is cached once it has been calculated so it need not be
## recomputed on subsequent inversion requests.

cacheSolve <- function(x, ...) {
  ##  Get the inverse field on the matrix object.
  inv <- x$getinverse()
  ##  If the inverse has aldready been calculated, then return the inverse.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##  If the inverse hasn't been calculated, get the data, caluclate the inverse,
  ##  save the inverse and return the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
