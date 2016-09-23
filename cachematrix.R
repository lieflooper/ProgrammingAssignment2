#creates the list of functions set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
        
        #assignes the variable inv a NULL
        inv <- NULL
        
        #creates a function that takes the matrix as an argument and assigned 
        #       it to a var y and assigns inv NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Returns the input argument / matrix
        get <- function() x
        
        # Assigns the inverse of the matrix to setinv by retrieving the results
        #       of solve from the cacheSolve function
        setinv <- function(inverse) inv <<- inverse
        
        # Simply returns the inverse stored in the var inv
        getinv <- function() inv
        
        # Returns a list of functions from above
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function takes as an argument a list of functions created 
#       by makeCacheMatrix.R

cacheSolve <- function(z, ...) {
        
        # First retrieves the inverse of the 
        inv <- z$getinv()
        
        # Checks to see if the inv is already assigned / calculated
        # if so, returns "retrieving cached data." and then returns what
        #       was in the cache
        if(!is.null(inv)) {
                
                message("retrieving cached data.")
                
                return(inv)
        }
        
        #retrieve matrix
        data <- z$get()
        
        #compute matrix inverse and assign to inv
        inv <- solve(data)
        
        #Use computed inverse, feed to setinv (which assignes computed inverse
        #       to variable inv in function(makeCacheMatrix.R))
        z$setinv(inv)
        
        inv
}