## makeCacheMatrix creates a special "matrix", which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    ## if matrix changes, the inverse changed too
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_) inverse <<- inverse_
  getinverse <- function() inverse
  
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}

## cacheSolve returns the inverse of the special matrix object
## If "matrix" object didn't change since last call of this function, the result will be returned from cache
cacheSolve <- function(x) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## extract from cache
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}
