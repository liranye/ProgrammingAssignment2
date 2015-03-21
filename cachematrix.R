## Put comments here that give an overall description of what your
## functions do -> in the code body

## Write a short comment describing this function
## Creating a new Matrix object based on a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             ## Initialize the inv variable to NULL so that
        ## the function doesn't hang the first time through.
        
        set <- function(y) {    ## Since this is some kind of a constructor, the instance which it will create
                ## will be able to set a new matrix as a content of this instance.
                
                x <<- y         ## This new content "belongs" to the instance's environment which is one level up from this constructor function
                ## so we must use super assignment so that this conent will be saved in the instance's environment.
                
                inv <<- NULL    ## We didn't yet calculated the new matrix inverse so in the instance environment we should also update the inv
                ## variable to NULL
                
        }
        get <- function() x     ## The instance will have a function to return it's matrix content
        setinv <- function(inverse) inv <<- inverse  ## The instance will have a function to set not this inv variable but the instance's inv variable
        getinv <- function() inv                     ## The instance will return inv variable
        list(set = set, get = get,                   ## This constructor create a list with the functions created above
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## -> in the code body

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                           ## Getting the instance's inv variable
        if(!is.null(inv)) {                         ## if it's not NULL just return the inv variable from the instance environment
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                             ## If inv is NULL, we get the matrix content of the instance 
        inv <- solve(data, ...)                     ## compute the inverse matrix using solve function,  
        x$setinv(inv)                               ## setting inv matrix  to be returned to the instance environment
        inv                                         ## and returning the inverse matrix
}
