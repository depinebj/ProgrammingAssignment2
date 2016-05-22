## These function will calculate the inversion of a matrix.  The inversion calculation  
## will only be re-calculated if the input matrix has changed or has not been calculated yet

## This function contains the functions of setmatrixinversion and getmatrixinversion that
## are used to create the matrix used in cacheSolve.  
## These functions store the matrix to be inverted and store and retrieve the inversion matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setmatrixinversion <- function(solve) m <<- solve
   getmatrixinversion <- function() m
   list(set = set, get = get,
        setmatrixinversion = setmatrixinversion,
        getmatrixinversion = getmatrixinversion)
}



## This function calculates the inversion of a matrix, and calls the functions in 
## makeCacheMatrix to retrieve the prior calculated inversion if the calling matrix
## has not changed and store the new inversion for future use

cacheSolve <- function(x, ...) {
   m <- x$getmatrixinversion()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setmatrixinversion(m)
   m
}

