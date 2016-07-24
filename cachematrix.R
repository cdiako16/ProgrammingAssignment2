## Put comments here that give an overall description of what your
## functions do
#__________________________________________________
# This assigment has two functions. The first one creates a "special" matrix
# object and can cache its inverse. The second one uses the output of the first
# function and computes the inverse if there is no prior inverse that can be cached 


## Write a short comment describing this function
##__________________________________________________

## This function (makeCacheMatrix) is able to create a matrix object and cache its inverse 
## by setting the matrix, getting the matrix, setting the inverse and getting the inverse

makeCacheMatrix <- function(x = matrix()) { 
        my_inv = NULL
        
        ## x and my_inv have been initialized above
        
        ## Assigning  y and NULL to x and my_inv in the parent environment
        
        set = function (y) {
                x <<- y
                my_inv <<- NULL
        }
        
        ## Retrieving x from the parent environment of makeCacheMatrix
        
        get = function() x
        
        
        # Getter and setter defined
        
        setinverse = function(inverse) my_inv <<- inverse
        getinverse = function () my_inv
        
        ## Assigning functions as elements within a list
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
##_________________________________________________

## This function computes the inverse of the matrix created by the first function above

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        my_inv = x$getinverse()
        if(!is.null(my_inv)) {
                message("Getting cached inverse")
                return(my_inv)
        }
        
        
        ## If the above condition is not true, the inverse of 'x' is calculated
        
        data <- x$get()
        my_inv <- solve(data, ...)
        x$setinverse(my_inv)
        my_inv
}
