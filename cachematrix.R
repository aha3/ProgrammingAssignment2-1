## The functions below take advantage of R's lexical scoping rules and reduces computation time by clearing the cache
## as needed and, when appropriate, computing the necessary function (inverse), and/or retrieving the cached data (inverse).


## This function creates a a special "matrix" object that can cache its inverse. This is done by initializing the 
## matrix object (x), and returning a list of named functions that are used downstream as the input in cacheSolve(). 
## The list contains named functions to: 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse<-function(inverse) inv<<- inverse
  getInverse<-function() inv
  list (set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function takes the makeCacheMatrix as its input and will either retrieve the cached inverse matrix (if it 
## has already been computed), or compute and return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
