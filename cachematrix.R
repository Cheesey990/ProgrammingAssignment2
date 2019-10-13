## These functions will create a special object that stores a matrix and caches its inversion.

## This function creates a special "matrix".

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      
      list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function solves the inversion of the special "matrix" then stores it in a cache.  If it has already
## been calculated and is stored in the cache, it will skip the calculation and recall the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
  
  }
