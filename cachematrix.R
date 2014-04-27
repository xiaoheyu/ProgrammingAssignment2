## There are two functions below. One is to create a "spectial" matrix
##that can cacge its inverse The second function computes the inverse

## create a matrix acctualy contains set,get,setinverse and
##getinverse functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ##set the value of matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##get the value of matrix
    get <- function() x
    ##set the inverse value of the matrix
    setinverse <- function(inverse) i <<- inverse
    ##get the inverse value of the matrix
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    
}


##function to determin wether the inverse of matrix is calculated
##if yes, return the stored value before, If not, calculate the
##inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ##determin wether it is null or not
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ##if not calculated before, calculate the inverse right now
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ##returen the inverse value
    i
}