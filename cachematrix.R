## These two functions provide the functionality to create a matrix, store it in an object, calculate its inverse in a given time and 
## store it also in the object. In this way, the first time the inverse is required it is calculated and storerd it. The next times
## the already calculated inverse matrix is returned from the object. If the original matrix is changed, the inverse is cleared and
## it will be calculated the next time the solver function is called.
## -----------------------------------------------------------------------------------------------------------------------------------

## The makeCacheMatrix function creates the CacheMatrix object and store the original matrix and also has a variable to store the
## inverse matrix when it is calculated through the cacheSolve function. This function itself creates two functions to set and get
## the original matrix and two functions to set and get the inverse matrix.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(n){
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is used to return the inverse of the original matrix. First it checks if the inverse was already calculated and if so
## it return the stored inverse matrix and it prints a text in the console specifying it returned an stored inverse matrix. If the
## inverse was not caluculated before, it uses the "solve" R function to calculate the inverse matrix and it stores it in the "m" object
## it received as a parameter.

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'm'
    inv <- m$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setInverse(inv)
    inv
}