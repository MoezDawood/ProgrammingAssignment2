## Overall the below pair of functions calculates the inverses of matrices or retrieves the inverses of matrices from caches in a time efficient manner
## The function makeCacheMatrix creates a list ("the special 'matrix' object") of functions that can cache the inverse of a matrix.

makeCacheMatrix <- function(m = matrix()) 
{
  inverse_m <- NULL
  set <- function(y) 
  {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_m <<-inverse
  getinverse <- function() inverse_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve either calculates the inverse of the matrix returned by makeCacheMatrix() or, if the inverse has already been calculated, retrieves it from the cache.

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if (!is.null(m)) 
  {
    message("GETTING CACHED INVERSE MATRIX")
    return(m)
  } 
  m <- solve(x$get())
  x$setinverse(m)
  m
}
