## The functions here allow to avoid potentially 
## time-consuming computations of inverse matrices.
## Once an inverse matrix is calculated, it is cached and can be
## retrieved if needed further

## Create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #When new value assigned to a matrix, inverse matrix 
        #is set to NULL
    set <- function(y) {
        x <<- y
        invers <<- NULL
    }
    get <- function() x
        ##Cache inverse matrix when it is set
    setinv <- function(inversMatrix) invers <<- inversMatrix
    getinv <- function() invers
        ##Create new object by returning a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of the matrix, retrieving the inverse from
##cache when possible

cacheSolve <- function(x = matrix(),...) {
        ## Return a matrix that is the inverse of 'x'
    invers <- x$getinv()
        ## Return cached data if not NULL
    if(!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
        ## Calculate inverse matrix otherwise and store in x$invers
    matrixdata <- x$get()
    invers <- solve(matrixdata,...)
    x$setinv (invers)
    invers    #The output of the function is the reverse matrix
}
