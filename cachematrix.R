## Put comments here that give an overall description of what your
## functions do

## creats a chache object for matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setinverse <- function(inverse){
        inv <- inverse
    }
    
    getinverse <- function(){
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached inverse")
        return(inverse)
    }
    mat <- x$get()
    inverse <- solve(mat)
    x$setinverse(inverse)
    inverse
}
