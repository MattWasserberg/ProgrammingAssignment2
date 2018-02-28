## Put comments here that give an overall description of what your
## functions do

## MW 28/02/2018
## makeCacheMatrix:
##      Create a vector containing four functions to
##      invert and store the results of matrix x
##cachesolve:
##      Check makeCacheMatrix for cached inverted matrix
##      Return cached matrix if available, otherwise get data from
##      makeCacheMatrix and invert

## Further notes: designed to work for a square matrix only
        
## Write a short comment describing this function

## MW 28/02/2018
## 1. set matrix (using <<- to allow operator to modify variables in parent environments)
## 2. get matrix
## 3. set inverse of matrix using the solve function
## 4. get the inverted matrix
## 5. create vector of values that be retrieved in other functions using 'list'

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setiv <- function(solve) iv <<- solve(x)
        getiv <- function() iv
        list(set = set, get = get,
             setiv = setiv,
             getiv = getiv)
}



## Write a short comment describing this function

## MW 28/02/2018
## 1. retrieve inverted matrix from makecachematrix
## 2. assess whether the inverted matrix is populated (i.e. there is something stored in the cache)
## 3. if there is, get the cached inverse matrix, else retrieve the non-inverse matrix and run the 
##    function (set iv) to invert it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getiv()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        mdata <- x$get()
        iv <- solve(mdata, ...)
        x$setiv(iv)
        print(iv)

        
}
