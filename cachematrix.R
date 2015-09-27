## The following code creates a list of functions
## for a matrix object, find and cache the inverse of that matrix


## makeCacheMatrix takes a matrix as an input and returns a list of functions
## for that matrix. These functions store and return the matrix and store and
## return the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function takes a matrix input and checks if the inverse
## already exists in the cache, if it does it returns the inverse from the 
## cache. If not, it calculates inverse by calling solve function and 
## stores that in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
