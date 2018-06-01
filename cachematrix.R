## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a list to
# set the value of the matrix
# get the value of the matrix
# set the value of the inv matrix
# get the value of the inv matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## This function computed the inverse of the matrix from makeCacheMatrix
## If already calculated the cacheSolve will retieve the inverse from the cache
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}



