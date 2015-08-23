#makeCacheMatrix takes a matrix argument x and creates a list
#containing the matrix and its inverse info. If cacheSolve is 
#used to find the inverse, the corresponding makeCacheMatrix is
#updated such that if we try to find the inverse of the same 
#again, it is just obtained from the cached list, instead of 
#being deterermined from scratch again.


#takes a matrix and produces a list with info abou the inverse
makeCacheMatrix <- function(x=matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) I <<- Inverse
    getInverse <- function() I
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#takes list and gets the inverse if it's calculated before,
#else it deermines it and saves it in the list 
cacheSolve <- function(x, ...){
    I <- x$getInverse()
    if(!is.null(I)){
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data,...)
    x$setInverse(I)
    I  # Return a matrix that is the inverse of 'x'
   
}