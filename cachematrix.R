## This functions will help a programme to not run multiple tedious R functions by
## saving the values in cache of the programme

## This function can set-up a list for the methods of setting,retriving a 
##matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  
  inv <- NULL
  
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Justlike vector mean retrival function, we can check for any cached value of the 
## inverse else compute and set it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## If nothing found in cached data, then we setup the inverse
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  
  inverse
}
