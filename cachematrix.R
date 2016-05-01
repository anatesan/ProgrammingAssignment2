## Cache the inverse of a matrix.   For the purposes of this assignment,  we assume that any matrix being passed in is invertable
## 

## makeCacheMatrix returns a list of functions (methods) on a cached matrix structure that avoids repeated matrix inverse operations
## List returns get, set, getinv(erse) and setinv(erse) methods for callers to use

makeCacheMatrix <- function(x = matrix() ) { # matrix whose inverse is to be cached
      inv <- NULL;              # the cached matrix inverse if non-null
      set <- function(new_x) {  # associates a matrix with this cached structure
            x <<- new_x
            inv<<- NULL
      }
      setinv <- function(new_inv) inv<<-new_inv  # sets the inverse matrix in cache strucure
      getinv <- function () inv                  # returns cached matrix inverse or null if not cached
      get <- function() x                        # gets original matrix from cache - note:  not the inverse
      
      # return list of the functions/methods on the CacheMatrix
      
      list(set = set, setinv=setinv, getinv=getinv, get=get)

}


## CacheSolve returns the matrix inverse.  It first checks to see if a cached matrix inverse is available & returns it if yes.
## Otherwise the matrix inverse is computed and stored in the CachedMatrix structure

cacheSolve <- function(x, ...) { ## x should be a list returned by makeCacheMatrix function
      ## Return a matrix that is the inverse of cached matrix stored in x
      
      # get the cached matrix inverse if available
      my_inv <- x$getinv()
      if (!is.null(my_inv)) {
            message("Getting cached matrix inverse")
            return(my_inv)
            
      }
      
      # compute the matrix inverse & cache it for future use
      
      my_mat <- x$get()
      my_inv <- solve(my_mat)
      x$setinv(my_inv)
      
      my_inv # return computed matrix inverse
}
