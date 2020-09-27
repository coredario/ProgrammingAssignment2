## makeCacheMatrix function creates a special matrix which is
## a list containing this functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matri

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

## cacheSolve function calculates the inverse of the special matrix
## created with the makeCacheMatrix function.
## First checks to see if the inverse has already been calculated.
## If it was calculated before, then it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse and sets
## that value in the cache with the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
