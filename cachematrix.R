## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix. 

## Your assignment is to write a pair of functions that cache the inverse of a matrix. 
## For this assignment, assume that the matrix supplied is always invertible 

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        
        get <- function() x 
        setinverse <- function(inverse) inv <<-inverse
        getinverse <- function() inv 
        list (set = set, 
              get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)      
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache. Computer the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse. 
## FOr this assignment, assume that hte matrix supplied is always invertible. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Retrieving cached data...")
                return(inv) 
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv) 
        inv
}
