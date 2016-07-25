## Functions used to create a special object that stores
## an invertible matrix and cache's its inverse

## Creates a list containing functions to:
## 1.Set matrix
## 2.Get matrix
## 3.Set matrix's inverse
## 4.Get matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse of the matrix created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
