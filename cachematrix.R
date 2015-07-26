## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function receives a vector created with the previous function 
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    ## Check if there is a not null value stored in m
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}

## This function gets a matrix and returns de inverse matrix, 
## looking first if this is stored in cache
cachematrix <- function (matriz = matrix()) {
    temp <- makeCacheMatrix (matriz)
    
    cacheSolve(temp)
}