##
## R Programming Assignment 2
## ~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## The solution is a minor adaption of the examples provided with the 
## assignment. The cache logic is identical. The data types change from
## a vector of numbers and a mean to a matrix and its inverse. The 
## mean() function is replaced by solve. And there is some renaming.
##
## +------------------------------+-----------------------------------+
## | Example Function             | Equivalent in assignment solution |
## +------------------------------+-----------------------------------+
## | makeVector                   | makeCacheMatrix. Takes a matrix as|
## |                              |   input rather than a vector.     |
## | get                          | get. Code is unchanged from the   |
## |                              |   sample.                         |
## | set                          | set. Code is unchanged from the   |
## |                              |   sample.                         |
## | getmean                      | getinverse. Function name and     |
## |                              |   variable passed to it changed   |
## | setmean                      | setinverse. Function name and     |
## |                              |   variable passed to it changed   |
## | cachemean                    | cachesolve. Calls "solve()" to get|
## |                              |   inverse rather than mean() to   |
## |                              |   get mean. Uses the              |
## |                              |   makeMatrixCache$getinverse()    |
## |                              |   function rather than get mean   |
## +------------------------------+-----------------------------------+

##---------------------------------------------------------------------
## makeCacheMatrix
##---------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##---------------------------------------------------------------------
## cacheSolve
##---------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}