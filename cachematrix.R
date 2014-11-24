## These functions cache the inverse of a matrix.

## Function to create a "matrix" object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Create a new "matrix" object
  ## Reset the local value of the inverse to NULL.
  m <- NULL
  ## Defines the set method.  This will set the value of the matrix in the parent environment.
  ## It will also reset the value of the inverse (m), in the parent environment, to NULL.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Defines the get method.  This will return the value of the matrix.
  get <- function() x
  ## Defines the set method for the inverse.  Sets the matrix inverse in the parent environment.
  setinverse <- function(inverse) m <<- inverse
  ## Defines the get method for the inverse.  Gets the matrix inverse from the parent environment.
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the "matrix" object returned by makeCacheMatrix.
## If the inverse has already been calculated, and the matrix has not changed,
## Then the inverse will be retrieved from the cache and returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If the matrix inverse is not null, then return the value from the cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the matrix inverse was null, then get the value of the matrix, solve for its inverse
  ## and then store in the parent environment (cache).  Finally, return the value.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}