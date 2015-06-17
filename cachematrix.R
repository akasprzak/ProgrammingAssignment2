## Following two functions are responsible for inversing provided matric and 
## caching the result

## The following function return a list of function for:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invVal) inv <<- invVal
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function inverses a matrix created with function 
## makeCacheMatrix, but in the first place checking if the matrix has been 
## already inversed.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
