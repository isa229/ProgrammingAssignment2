## Put comments here that give an overall description of what your
## functions do
## This function allows me to create a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
  inv <- matrix()     
  set <- function(y) {
        x <<- y
        inv <<- matrix()
  }
        
  get <- function() x     
  set_inv <- function(invert) inv <<- invert    
  get_inv <- function() inv
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  inv <- x$get_inv()
        
  if(!all(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
        
  inv
}
