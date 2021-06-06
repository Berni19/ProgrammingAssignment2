## Demonstration of the concept of lexical scoping. The <<- operator 
## can be used to assign a value to an object in an environment 
## other than the current environment. The following two functions 
## are used to create a special object that stores a matrix and caches 
## its inverse.

## makeCacheMatrix creates a special "matrix" object that stores a  
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y){ 
    x <<- y       
    inv <<- NULL 
  }
  get <- function() x 
  setinverse <- function(solve) inv <<- solve 
  getinverse <- function() inv 
  list(set = set, get = get,   
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special 
## "matrix" created by makeCacheMatrix. If the inverse 
## has already been calculated and the matrix remains unchanged, 
## cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() 
  if(!is.null(inv)) {   
    message("getting cached data")
    return(inv)                   
  }
  data <- x$get()         
  inv <- solve(data, ...) 
  x$setinverse(inv)       
  inv                      
}


