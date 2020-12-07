#### Assignment: Caching the Inverse of a (square) Matrix

### makeCacheMatrix`function creates a special "matrix" object
### that can cache its inverse.

### initialising objects x and inv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
### setting the value of matrix using assignment operator
  set <- function(y) {
    x <<- y
    
### clears the inv value if previously cached
    inv <<- NULL
  }
### getting the value of matrix
  get <- function() x
  
### setting the value of inversion using assignment operator   
  setinv <- function(solve) inv <<- solve
  
### getting the value of inversion (using lexical scoping to find the inv value)
  getinv <- function() inv
  
## creating list and assigning names to the list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }
  
### cacheSolve`function computes the inverse of the special
### "matrix" returned by `makeCacheMatrix` above. 
### If the inverse has already been calculated (and the matrix has not changed), then
### `cacheSolve` should retrieve the inverse from the cache.
### The argument of cacheSolve is the object returned from makeCacheMatrix. 
### Atomic vector as argument will generate an error. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()

### If the inverse has already been calculated (inv is not NULL) (and x has not changed), 
### then cacheSolve` retrieves the inverse from the cache and the message is returned.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
### If the inverse has not been calculated (inv is NULL), 
### function gets the value of matrix from the input object (argument)
  data <- x$get()
### and calculates the inversion
  inv <- solve(data, ...)
### and sets the inversion in the input object (agrument)
  x$setinv(inv)
### lastly it returns the inversion of x
  inv
}


### Simple test to test the makeCacheMatrix(x) and cacheSolve(CacheMatrix)
### x = matrix(rnorm(1:25), nrow=5, ncol=5)
### source('cachematrix.R')
### CacheMatrix <- makeCacheMatrix(x)
### cacheSolve(CacheMatrix)
### [value is returned here]
### cacheSolve(CacheMatrix)
### getting cached data #message is generated that the value is retrived from cache
### [value is returned here]