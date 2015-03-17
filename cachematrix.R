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
# This first core function returns a list of functions 
# assocated with the matrix that operate on the value and the cached
# results.
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
    inv<-NULL               #initially set the cached result to nothing
    
    setMatrix<-function(y) {
        x<<-y               # Overwite the original matix and
        inv<<-NULL          # reset the cached result to nothing
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
# If the value had already been calculated and cached, then the 
# cached value is simply returned
# 
# Args:
#   x: the list returned by the makeCacheMatrix function
#   ...: additional parameters passed to the 'solve' function
#
# Returns:
#   The inverse matrix (from cache or newly calculated)    

cacheSolve <- function(x, ...) {
    inv<-x$getInverse()             # lget the cached value
    
    if (!is.null(inv)){             # if it exists, return it
        message("getting cached data")
        return(inv)        
    }
    
    sourceMatrix<-x$getMatrix()     # if not cached, calculate the inverse
    inv<-solve(sourceMatrix,...)    
    x$setInverse(inv)               # cache the result
}
#-----------------------------------------------------------------------
