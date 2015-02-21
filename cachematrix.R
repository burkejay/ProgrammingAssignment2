## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function of functions to keep a pointer to
## a matrix and a pointer to its inverse.  When matricies get large,
## calculating the inverse is expensive, so this code could oliviate
## the need to calculate the same matrix's inverse over and over

makeCacheMatrix <- function(x = matrix()) {
    ## when called for the first time, make a local variable called
    ## "i" that will point to the inverse
    i <- NULL
  
    ## starting fresh, set our cached matrix to x and reset the 
    ## inverse to null, potentially dumping an old inverse
    set <- function(y) {
        x <- y
        i <- NULL
    }
    
    ## use the function to request a reference to the cached matrix
    get <- function() x
    
    ## cache the inverse of the matrix, calculated elsewhere
    setInverse <- function(inv) i <<- inv
    
    ## use this function to request a reference to the cached
    ## matrix's inverse
    getInverse <- function()  i

    ## what gets returned when this function is called: a list of functions
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## override the matrix function solve with a function that will hold
## a matrix and its inverse.  This should be used if the same
## matrix's inverse will be re-calculated repeatedly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## if the getInverse function returns a matrix,
        ## it came from the cache, just return it and
        ## end the function
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
 
        ## else the getInverse function returns a null,
        ## so calculate the inverse and cache the results
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(i)
        
        ## return the inverse
        i
}
