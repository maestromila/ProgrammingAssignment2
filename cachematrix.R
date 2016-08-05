## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix returns a list containing:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## List, which is output of the function, is used as input for the function cacheSolve.
 
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  set_inverse = function(inverse) inv <<- inverse 
  get_inverse = function() inv
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

## Write a short comment describing this function
## Function cacheSolve calculates the inverse of makeCacheMatrix() using the input from makeCacheMatrix function above.
## The output of this function is inverse of the original matrix.
## If the inverse was calculates earlier then the function returns the cached inverse matrix, instead of calculating inverse again. If not then
## the function calculates the inverse and sets the value of the inverse in the cache via setinv function.

cacheSolve <- function(x, ...) {
  inv = x$get_inverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dt = x$get()
  inv = solve(dt, ...)
  x$set_inverse(inv)
  return(inv)
}
