## A pair of functions that cache the inverse of a matrix
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

> source("MakeCacheMatrix.R")
pmatrix <- makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))

pmatrix$get()
     [,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14
[3,]    3    7   11   15
[4,]    4    8   12   16

pmatrix$getInverse
NULL
cacheSolve(pmatrix)
[,1] [,2]
[1,] -2 1.5
[2,] 1 -0.5
cacheSolve(pmatrix)
getting cached data
[,1] [,2]
[1,] -2 1.5
[2,] 1 -0.5
pmatrix$getInverse()
[,1] [,2]
[1,] -2 1.5
[2,] 1 -0.5
