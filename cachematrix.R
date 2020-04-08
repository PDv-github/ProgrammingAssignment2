## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# initiates a function called makeCacheMatrix that requires as argument a matrix x.
# This function creates and stores an object that contains  x, invMatrix and the functions set(),
# get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invMatrix <<- solve
  getinverse <- function() invMatrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

# this function rerieves x from the object created by makeCacheMatrix, then checks if a value
# is already stored in memory. If this object exists, the function will return the value.
# if not, the function will calculate the inverse of the x matrix.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()
  if (!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  values <- x$get()
  invMatrix <- solve(values, ...)
  x$setinverse(invMatrix)
  invMatrix
}

