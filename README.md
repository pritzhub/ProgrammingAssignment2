# ProgrammingAssignment2
######
# ProgrammingAssignment2
Assignment: Caching the Inverse of a Matrix

Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

Write the following functions:

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

Code:
##Inverse of a matrix could be a costly computation; to overcome this issue, the results of a first run of a matrix inverse can be stored in the cache and
##if there is no change in the matrix then for sub-sequent runs, instead of recalculating the inverse, 
##value can be directly taken from the cache. 
#Following functions cache the inverse of a matrix and retrice results from the cache.

## makeCacheMatrix function stores the matrix and the cached value of matrix. The return value of this
## function is a list of four functions
## setmatrix    This function sets the value of matrix
## getmatrix    This function gets the value of matrix
## setinverseincache    This function sets the inverse value in the cache
## getinversefromcache  This function gets the cached value


makeCacheMatrix <- function(x = matrix()) {
        ## Assign inverse variable to blank
        invcache <- NULL
        
        setmatrix <- function(y) {
                ## assign a value to a matrix, since different value is being assigned
                ## to the matrix cached value has to be cleaned/set to NULL
                x <<- y
                invcache <<- NULL
        }
        
        getmatrix <- function() {
                ## returns the matrix value
                x
        }
        
        setinverseincache <- function(invval) {
                ## given value of the matrix is set in the cache
                invcache <<- invval
        }
        
        getinversefromcache <- function() {
                ## returns the cache value
                invcache
        }
        # function retruning the list of functions
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverseincache = setinverseincache, getinversefromcache = getinversefromcache)
}




## Function cacheSolve calculates the inverse of the matrix which was set through the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## get the value stored in the cache
        invcache <- x$getinversefromcache()
        
        ## checking if cache exists or not. If cache exists then display a message and retrun the cache value
        if(!is.null(invcache)) {
                message("getting cached matrix")
                return(invcache)
        }
        
        ## get the matrix value
        data <- x$getmatrix()
        
        ## inverse the matrix
        invcache <- solve(data, ...)
        
        x$setinverseincache(invcache)
        
        ## return the inverse value
        invcache        
}
