## The following pair of functions is intended to reduce the computational
## time required when repeatedly calculated the inverse of an invertible
## matrix. After the inverse matrix has been calculated, it is stored
## in the cache for easy retrieval later.

## The following function creates a list that contains four functions:
## 1. A function to (re)set the argument matrix
## 2. A function to retrieve (i.e., return) the argument matrix
## 3. A function to (re)set (i.e., store) the inverse of the argument matrix
## 4. A function to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function takes the output of the previous function as an
## argument. If the inverse of the input matrix has not yet been calculated,
## this function computes it and stores it. Otherwise, the function returns
## the (previously-calculated) inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}