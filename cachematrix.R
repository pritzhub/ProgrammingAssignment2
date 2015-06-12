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
