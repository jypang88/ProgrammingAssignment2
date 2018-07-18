## This function creates two functions: makeCacheMatrix and cacheSolve. 
## The two functions are used together to create special matrix objects which are able to cache their own inverse matrix.   
## This avoids re-computing the inverse of the matrix if it has already been computed before.

## makeCacheMatrix creates the special matrix object. 
## It returns a list with separate functions to set the matrix, retrieve the matrix, set its inverse, and retrieve its inverse.
## The function makes use of lexical scoping to cache its inverse in component functions' defining environment.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # Function to set matrix and wipe cache.        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function to retrieve matrix.
        get <- function() x
        
        # Function to store matrix inverse in cache.
        setinv <- function(inverse) inv <<- inverse
        
        # Function to retrieve cached matrix inverse.        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the special matrix object created by makeCacheMatrix. 
## If the matrix's inverse was not found previously, cacheSolve calls the regular solve() function.
## If the matrix's inverse was already computed, then cacheSolve retrieves the inverse from the cache of the matrix object.

cacheSolve <- function(x, ...) {
        # Returns value of matrix inverse cache 
        inv <- x$getinv()
        
        # Checks whether the inverse has already been cached. 
        # If yes, returns cached inverse and exits function.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Retrieves matrix to compute inverse.
        data <- x$get()
        
        # Computes inverse.
        inv <- solve(data, ...)
        
        # Stores inverse in cache.
        x$setinv(inv)
        
        inv
}
