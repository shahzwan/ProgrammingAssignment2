## A pair of function that cache the inverse of a matrix

# Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(matrix){
                m<<- matrix
                m<<- NULL
        }
        get <-function() m
        
        setInverse <- function(inverse) i<<-inverse
        
        getInverse <- function() i
        
        list(set = set, get= get,
                setInverse = setInverse,
                getInverse= getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data<- x$get()
        
        m<-solve(data)%*%data
        
        x$setInverse(m)
        
        m
}
