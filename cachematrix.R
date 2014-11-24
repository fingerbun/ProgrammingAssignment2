##
## R Programming Assignment 2
## ~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## The solution is a minor adaption of the examples provided with the 
## assignment. The cache logic is identical. The data types change from
## a vector of numbers and a mean to a matrix and its inverse. The 
## mean() function is replaced by solve(). And there is some renaming.
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
    # Saves the original matrix passed
    x <<- y
    m <<- NULL
  }
  # Retrieves the original matrix passed
  get <- function() x
  # Saves the inverse matrix passed
  setinverse <- function(inverse) m <<- inverse
  # Retrieves the inverse matrix passed
  getinverse <- function() m
  # Returns a list of the functions created above so we can use them.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##---------------------------------------------------------------------
## cacheSolve
## Paramater passed is the reture from makeCacheMatrix. We use the
## functions we wrote there to do our cacheing and retrieval
##---------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  # Get the inverse
  m <- x$getinverse()
  # if m is not null we have previously saved an
  # inverse and can retrieve and return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if m is null we don't have the inverse in cache. We have
  # to calculate and return it. Bet we use our setinverse
  # function to cache it for next time.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}