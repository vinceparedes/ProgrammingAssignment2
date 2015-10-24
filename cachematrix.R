## These two functions together allow a user to create and reuse matrix inverses. Because creating the
## inverse of a large matrix can be expensive, these functions allow a user to retrive a cached (precalculated)
## version of the matrix. The user does not have to worry about whether the inverse has been precalculated or not
## because this is made automatic by these functions. 

## This function takes as input a matrix of numeric values. It then returns a list of 4
## functions that can be used to get and set the matrix, and to get and set the inverse of the
## matrix. 
## makeCacheMatrix needs to be called before using the cacheSolve function. 
## makeCacheMatrix does not need to be called with an argument but at some point the set function needs to be used.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## four functions defined below: set, get, setInverse, getInverse
    set <- function(y) {
        ## x and i are made global variables, that are now accessible to the cacheSolve function 
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(theInverse) i <<- theInverse
    getInverse <- function() { i }
    ## return the four functions in a list. 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function uses functions in the makeCacheMatrix function.
## This function returns the inverse of the input parameter (a matrix). 
## cacheSolve is special because it returns a precalculated inverse, or if there is no precalculated
##    version of the matrix, it calculates the inverse and stores it for later use. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get the inverse
    i <- x$getInverse()
    ## If the inverse is not null then return the inverse
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    ## if the inverse _is_ null, compute the inverse and save it for later use. Then return the inverse.
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
