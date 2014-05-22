## The function 'cacheSolve()' calculates, stores, and returns the inverse of
## any square matrix using a list of functions provided by the
## function 'makeCacheMatrix()' for a given square matrix. The calculation
## is performed just once on each square matrix of interest and each resulting
## inverse matrix stored in its own unique cache to enable subsequent retrievals 
## by 'cacheSolve()' without having to repeat the calculation.

## 'makeCacheMatrix()' takes any numeric or complex square matrix as its
## argument and returns a list of four functions:
##
##   'set()' assigns the square matrix to 'x' and a NULL value to 'inv'
##    in the parent environment of 'makeCacheMatrix'.
##
##   'get()' returns the matrix stored in 'x'.
##
##   'setsolve()' assigns the value of its argument to 'inv'
##   in the parent environment of 'makeCacheMatrix'.
##
##   'getsolve()' returns the matrix stored in 'inv'.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setsolve <- function(solve){
          inv <<- solve
     }
     getsolve <- function() inv
     list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)      
}

## 'cacheSolve()' takes as its argument a list of functions generated
## by 'makeCacheMatrix()' for a given square matrix and returns the
## inverse of that square matrix. Using these functions, the 'cacheSolve()'
## function checks whether or not an inverse matrix has been cached already
## for the square matrix; if not, it calculates the inverse using
## the R base package's 'solve()' function and stores the resulting
## inverse in its own unique cache before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getsolve()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve (data, ...)
     x$setsolve(inv)
     inv
}