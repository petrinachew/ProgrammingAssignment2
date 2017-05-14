## Programming Assignment 2: Lexical Scoping
## Caching the inverse of a matrix

## A pair of functions are made in order to cache the inverse of a matrix. 
## Matrix inversion is usually a costly computation, hence the pair of functions are made to simplify this.
## The functions are: makeCacheMatrix and cacheSolve

## makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }
     data <- x$get()
     i <- solve(data, ...) #if x is a square invertible matrix, then solve(x) returns its inverse.
     x$setinverse(i)
     i
}

## Testing if the functions work
# Check/Change the working directory using getwd() and setwd()
source("ProgrammingAssignment2/cachematrix.R")
# Create a matrix to test
mat <- makeCacheMatrix(matrix(1:4, 2, 2))
mat$get()
cacheSolve(mat)
mat$getinverse()