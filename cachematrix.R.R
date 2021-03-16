## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The function makeCacheMatrix() creates a vector, which contains a function 
#in which the functions for the matrix set() and get() are defined, but at the 
#same time the behaviour of the inverse matrix is established, also the 
#functions getInverse() and setInverse() are defined.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

#This function returns the inverse matrix after the computation defined 
#by the previous function. In case the matrix is already inverted,
#this function returns the inverse matrix from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}


