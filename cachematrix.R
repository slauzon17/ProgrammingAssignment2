## Put comments here that give an overall description of what your
## functions do
## Pair of functions that cashe the inverse of a matrix.


## Write a short comment describing this function
## Function creates a special "matrix" object that can cashe its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Write a short comment describing this function
## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been computed then the following casheSolve should retrieve the inverse from the cashe.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  
}
