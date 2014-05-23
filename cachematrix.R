# Programming Assignement 2

# The two functions created below will cache the inverse of a matrix
# therefore saving computing time every time the function is called.
# To do that, the matrix need to be invertible (square).

# First, the function "makeCacheMatrix" will create a matrix object and cache its inverse.

makeCacheMatrix <- function(m = matrix()) { 
        im <- NULL                            
        set <- function(y) {                    # set the value of the matrix defined in the "set" function                        
                m <<- y                 
                im <<- NULL
        }      
        get <- function() m                     # get the value of the matrix "m" and store it as "get"         
        setinv <- function(inv) im <<- inv      # set the the value of the inverse matrix "im" and store it as "setinv"          
        getinv <- function() im                 # get the value of the inverse matrix "inv" (from im) and store it as "getinv" 
        list(set = set, get = get,           
             setinv = setinv,     
             getinv = getinv)                   # list the functions required by "makeCacheMatrix" to create that matrix
}


# Now, to obtain the inverse of the matrix computed by the "makeCacheMatrix" function
# we will use the function "cacheSolve".
# However, if no change to the matrix were made then the inverse matrix should be retreived from the cache

cacheSolve <- function(m, ...) {
        im <- m$getinv()                        # Is the inverse matrix already cached?
        if(!is.null(im)) {   
                message("getting matrix from the cache")            
                return(im)        }        
        data <- m$get()                         # if matrix not cached, get it into "data"       
        im <- solve(data, ...)                  # the "solve" function compute the inverse of a square matrix       
        m$setinv(im)                            # the inverse matrix is cached and returned
        im 
}
