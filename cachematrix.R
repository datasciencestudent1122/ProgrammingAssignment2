## This function is a function which store 4 other functions as well
# as a matrix m.
# set() changes the matrix stored in the main function
# get() returns the matrix in the main function
# setinverse() stores an input value in m 
#    (may or may not be the inverse of the matrix stored in the main function)
# getinverse() returns the value in m 

makeCacheMatrix <- function(x = matrix()) {
  
  #initializes m
  m <- NULL
  
  #sets the stored matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #gets the stored matrix
  get <- function() x
  
  #sets m
  setinverse <- function(solve) m <<- solve
  
  #gets m
  getinverse <- function() m
  
  #lists the 4 functions so they are available to be
  # accessed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function returns the inverse of a "special matrix" created
# from makeCacheMatrix(). 
# First, it will retrive m if it has a stored value. 
# Otherwise, it will get the matrix stored in makeCacheMatrix(x=matrix())
# and calculate the inverse, set the inverse, return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## If m is not empty, return m, whatever it is set to.
  if(!is.null(m)) {
    message("getting stored data")
    return(m)
  }
  
  # If the matrix inverse has not been calculated,
  # get the matrix stored in makeCacheMatrix(), 
  # calculate the inverse, set the inverse, return the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#Example 1 - solves the matrix's inverse and returns it
b <- matrix(c(2,3,1,5,1,6,9,1,10),nrow=3,ncol=3)
c<- makeCacheMatrix(b)
cacheSolve(c)

#Example 2 - sets and gets m to an arbitrary matrix
d <- matrix(c(2,1,2,57),nrow=2,ncol=2)
c$setinverse(d)
c$getinverse()

#Example 3 - gets original input matrix as it
# has not been changed
c$get()

#Example 4 - since m was set in example 2, it returns
# that value
cacheSolve(c)
