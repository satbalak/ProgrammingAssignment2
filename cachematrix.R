## Source this file if you frequently calculate inverse if a matrix
## Use makeCacheMatrix to store the matrix in an list
## Use cacheSolve on the output of makeCacheMatrix
## On first usage of cacheSolve, the inverse is calculated and stored
## On subsequent usages of cacheSolve, the stored inverse is just returned,
## without actually "solve"ing matrix

################################################################################
## makeCacheMatrix takes a matrix as input and returns
## four functions as a list. The functions are
## get: returns the stored data
## set: to reset the matrix in a stored object
## getinverse: to get the inverse of the matrix
## setinverse: to set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## xi is the inverse of matrix x
    ## We set xi to NULL first
    xi <- matrix(data=NA, nrow=nrow(x), ncol=ncol(x))
    
    ## The set function gives a value to x and resets the inverse
    set <- function(y) {
        x <<- y
        xi <<- matrix(data=NA, nrow=nrow(x), ncol=ncol(x))
    }
    
    ## The get function just returns the value
    get <- function() { x }
    
    ## The setinverse store the inverse of x by superassignment
    setinverse <- function(solve) { xi <<- solve }
    
    ## getinverse returns the inverse of m
    getinverse <- function() { xi }
    
    ## Now, return a list of the 4 functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

################################################################################
## cacheSolve takes the output of makeCacheMatrix as input
## when you invoke cacheSolve, you can pass other parameters to the solve
## function in place of "..."
## cacheSolve checks if the matrix inverse exists, before actually computing
## the inverse. If the matrix inverse exists, then it just returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    xi <- x$getinverse()
    if (!all(is.na(xi))) {
        ## if control comes here, that means the inverse is already caches
        message("getting cached data")
        return(xi) # return and get out of the function
    }
    ## if control comes here, that means the function is called
    ## for the first time and we need to cache the inverse
    data <- x$get()
    xi <- solve(data, ...) # get the inverse
    x$setinverse(xi)
    xi
}
