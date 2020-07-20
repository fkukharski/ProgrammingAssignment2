## Following functions cache the inverse matrix

## This function creates a "martix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## inv - inverse matrix
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function return a matrix that is inverse to argument
## If the inverse matrix exists in cache, then the function get it from here
## If not, the function calculate the inverse matrix and save it in cache

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
