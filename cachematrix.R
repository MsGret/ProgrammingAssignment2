## Copy+Paste comment (small changes in the Assignment text):
## In this Programming Assignment we apply the <<- operator which
## can be used to assign a value to an object in an environment that
## is different from the current environment. Below are two functions
## that are used to create a special object that stores
## a matrix and cache's its inverse.

## The `makeCacheMatrix` function creates a special "matrix" object
## that can cache its inverse:
## 1. `set` - set the matrix
## 2. `get` - get the matrix
## 3. `setInverse` - set the inverse of matrix
## 4. `getInverse` - get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The `cacheSolve` function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}