## A pair of functions that can cache the inverse of a matrix.

## This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## "makeCacheMatrix", and retrieves it from the cache if has already been
## calculated.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cache data")
            return(inv)
      }
      getmatrix <- x$get()
      inv <- solve(getmatrix, ...)
      x$setinv(inv)
      inv
}

# prueba
