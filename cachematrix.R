#------------------------------------------------------------------------
# These functions together enable the caching of the results for 
# the cpu intensive matrix inversion function known as 'Solve'

# Example Usage:
#
# Firstly set the original matrix where 'm' is the source matrix
# and 'z' is the returned list containing the caching functions
#   > z<-makeCacheMatrix(m)
# 
# As required use the 'cacheSolve' function to retrieve the cached value
#   > u<-cacheSolve(z)
#
# If you need to modify the original matrix (which will reset the inverse)
#   > z$setMatrix(new_matrix)

#-----------------------------------------------------------------------

# Function makeCacheMatrix:
# This first function returns a list comprising other functions 
# that set and get the original matrix and the cached inverse.
# 
# Args:
#   x: the source matrix
#
# Returns:
#   $getMatrix()            : Returns the original matrix
#   $setMatrix(y=matrix())  : Overwrites the original martix with another
#   $setInverse(y=matrix()) : sets the cached value of solve function
#   $getInverse()           : Gets the cached value of the sove function

makeCacheMatrix <- function(x = matrix()) {
    
    #initially set the cached result to nothing
    
    inv<-NULL               
    
    
    # Overwite the original matix and reset the cached result to nothing
    
    setMatrix<-function(y) {
        x<<-y               
        inv<<-NULL           
    }
    
    
    getMatrix<-function() x         # Return the original matrix
    setInverse<-function(y) inv<<-y # Set the cached value
    getInverse<-function() inv      # Return the cached value
    
    
    #Create the list of functions to return
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)  
}

#-----------------------------------------------------------------------

# Function cacheSolve:
# This second function performs the solve operation on the matrix.
# If a cached value exits, that is returned else a new inverse is 
# calculated, cached and then returned
# 
# Args:
#   x: the list returned by the makeCacheMatrix function
#   ...: additional parameters passed to the 'solve' function
#
# Returns:
#   The inverse matrix (from cache or newly calculated)    

cacheSolve <- function(x, ...) {
    
    # get the cached value
    
    inv<-x$getInverse()             
    
    
    # if it exists, return it
    
    if (!is.null(inv)){             
        message("getting cached data")
        return(inv)        
    }
    
    
    # if not cached, calculate the inverse and cache the result
    
    sourceMatrix<-x$getMatrix()     
    inv<-solve(sourceMatrix,...)    
    x$setInverse(inv)               
}
#-----------------------------------------------------------------------
