## Function that caches a given matrix argument 
## the super assignment operator <<- makes variables visible to the outer scope of its defining environment
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
        
}


## Return a matrix that is the inverse of 'x'
## Checks for the cached inverse first and returns it
## If there's no cache, the matrix is solved, stored and then returned
cacheSolve <- function(x, ...) {

  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInv(inv)
  inv
  
}

