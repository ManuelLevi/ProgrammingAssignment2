## Assignment: Caching the Inverse of a Matrix
#"Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly"

##Very similar to the example function makeVector, this function, creates a list containing a functions to
#set and get the value of the matrix
#set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  lista = list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  return(lista)
}

##This function starts by checking if the inverse of the matrix already as been calculated.
#If true it returns the cached value for the inverse
#If not it calculates the inserse, caches it and returns it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  return(m)
}

##You can run this to test it.

test_it = function(){
  matr = replicate(1000, rnorm(1000)) 
  matr_c_ready = makeCacheMatrix(matr)
  message("running first time")
  m1 = cacheSolve(matr_c_ready)
  message("running second time")
  m2 = cacheSolve(matr_c_ready)
  message("end")
}