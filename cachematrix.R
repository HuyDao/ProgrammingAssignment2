## Conceptually, makeCacheMatrix function takes a matrix and then adds another property to it: the inverse.
## cacheSolve function takes in as argument the object that is created by the makeCacheMatrix function.
## cacheSolve then attempt to see if the input object already contains the inverse. If it sees a valid value then 
## it knows that the inverse was calculated previously. So instead of doing expensive inverse calculation, it gets
## the inverse property and returns right away.
## cacheSolve only calls the expensive "solve" operation only if it sees that the inverse was not previously calculated.
## After cacheSolve calls "solve", it would record the result in the inverse property of the input.
## The pattern of these 2 functions is very similar to the example makeVector and cacheMean functions.

## Essentially defines a new data type, which has the original matrix and place for its inverse as well as functions
## to get and set the inverse. i is the internal variable for inverse and getinverse() is the get function,
## setinverse() is the set function.
## In case the matrix is reassigned a different value, it would make sure to reset the inverse property,
## invalidating the "cache".
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function does "lazy calling" of the "solve" function, meaning it would only call "solve" when necessary.
## When the "solve" function was previously called, it would just return the cached value.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        ## Return a matrix that is the inverse of 'x'
}

