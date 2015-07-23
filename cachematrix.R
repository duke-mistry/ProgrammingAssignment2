## The following functions work together to calculate the 
## inverse of a matrix. The inverse is saved in a cache so
## that it can be reused in order to improve function speed.
## Based on coursera "R Programming" Assignment 2.


## makeCacheMatrix creates a list containing functions:
## 1) To set the value of the matrix 
## 2) To get the value of the matrix
## 3) To set the value of the inverse matrix
## 4) To get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()
                x
        setinv <- function(inv)
                m <<- inv
        getinv <- function()
                m
        list(
                set = set, get = get,
                setinv = setinv,
                getinv = getinv
        )
}


## cacheSolve calculates the inverse of the matrix created
## with makeCacheMatrix. First it checks to see if the inverse
## has already been calcualted. If so it returns the chached value
## and skips the rest of the function. If not it will calculate 
## the inverse and save the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()                    
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
