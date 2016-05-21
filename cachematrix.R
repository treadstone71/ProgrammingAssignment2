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
        inv <<- inverse
    }
    
    getinverse <- function(){
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns inverse of matrix if it is cached or else finds the inverse

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
