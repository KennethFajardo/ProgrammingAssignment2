## This function creates a cachable matrix by creating getters and setters
## which foreshadows object-oriented programming

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               #initialize m
    set <- function(y) {    #set the function for initializing local variables
        x <<- y
        m <<- NULL
    }
    get <- function() x     #get the function for initializing local variables
    setInv <- function(mat) m <<- solve(mat) #set the function for solving for the inverse
    getInv <- function() m  #get the function for solving for the inverse
    list(set = set, get = get, #return the functions made in makeCacheMatrix
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    # If the matrix is cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Else cache the new matrix and its inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m) #
    m
}
