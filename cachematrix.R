## This function allows me to create a matrix, get the stored matrix, set and get its inverse through another function.

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


## This function returns a matrix that is the inverse of a matrix 'x'. If it determines that the inverse was already computed, retrieves the stored inverse matrix. 

cacheSolve <- function(x, ...) {
        
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
