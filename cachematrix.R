## These functions compute the inverse of a matrix and store it in a cache
## The cached inverse can be recalled without recalculating it



## This function sets up the input matrix and functions which are operated on by cachesolve function below
makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL #set m to NULL to clear any old stored matrix
      set <- function(y) {  #set function can be used to change the value of the matrix without initializing another instance
            ##It is assumed, but not stated that y is a matrix
            x <<- y  #sets x in parent environment
            m <<- NULL #sets m in parent environment
      }
      get <- function() x  #function to get the input matrix
      setsolved <- function(solve) m <<- solve  #function to set the solved matrix
      getsolved <- function() m #function to get the solved matrix
      list(set = set, get = get,#returns a list of functions, this allows calling the functions from parent environment
           setsolved = setsolved,
           getsolved = getsolved)
}




## This function returns the cached inverse matrix.  
## If the matrix has not yet been solved, it solves and stores to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getsolved()
      if(!is.null(m)) {       #if m is not empty, get the cached value and return it
            message("getting cached data")
            return(m)
      }
      data <- x$get()         #if m IS empty, get the data using the get function
      m <- solve(data, ...)   #solve the matrix and set m to this value
      x$setsolved(m)          #use the setsolved function to write the solved matrix to cache
      m                       #returns m
}
