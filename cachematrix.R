## This function contains the functions to set the value of the 
## matrix, get the value of the matrix, set the inverse,
## and get the inverse. Then it lists all of these.


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


## This function calculates the inverse of the special matrix, that is
## assumed to be invertible using the function solve(X)

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
