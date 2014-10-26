## These functions take a matrix and cache the inverse value of that matrix so that it does
## not have to be recalculated repeatedly

## Takes the matrix argument and creates a list containing functions to set and
## get the matrix values and set and get the inverse matrix values

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
## Set matrix    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
## Get matrix    
    get <- function() x
    
## Set matrix inverse
    setinverse <- function(inverse) inv <<- inverse
    
## Get matrix inverse
    getinverse <- function() inv
    
## Return matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


## This function takes a matrix argument and returns its inverse
## If inverse has been calculated already the value is returned from cache

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()

## If matrix inverse already calculated then return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

## Calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)

## Return inverse
  inv
}
