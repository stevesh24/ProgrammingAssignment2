###########################################################################

## Overview
## Returns the inverse (m) of an input matrix (x). To save computing, m is 
## computed once with solve() and cached.
## On subsequent calls the cached inverse (m) is returned without computing. 

## Instructions
## 1. Call makeCacheMatrix(x) first to create list of matrix functions (the 
##    special matrix object).
##        e.g., matrix_object <- makeCacheMatrix(input_matrix)
## 2. Then call cacheSolve() 
##        e.g., inv_matrix <- cacheSolve(matrix_object)

###########################################################################

## makeCacheMatrix(): takes the input matrix (x) and creates a matrix 
## 'object'(list with set,get,setinv,getinv) to be used by cacheSolve
##    set: sets x to the input y matrix. sets the inverse matrix (m) to NULL.
##    get: gets x
##    setinv: finds inverse matrix (m) and assigns m to the inverse    
##    getinv: gets inverse matrix (m)    

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

###########################################################################

## cacheSolve() 
## returns the inverse of x. The input is the list (special matrix
## object) created by makeCacheMatrix(). 

cacheSolve <- function(x, ...) {

    ## first use the function getinv to put cached inverse into m, 
    ## and return m if it exists
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }

    ## Otherwise get the input matrix (x in makeCacheMatrix) with x$get, 
    ## use solve to find the inverse of the data matrix, and set inverse
    ## to m with the function setinv, and return m         
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
