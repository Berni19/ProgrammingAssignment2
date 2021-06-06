## Demonstration of the concept of lexical scoping. The <<- operator 
## can be used to assign a value to an object in an environment 
## other than the current environment. The following two functions 
## are used to create a special object that stores a matrix and caches 
## its inverse.

## makeCacheMatrix creates a special "matrix" object that stores a  
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) { ## x initialized as function arg &
                                            ## default value set to empty matrix
                                            ## to avoid x$get() error when
                                            ## executing cacheSolve.
    
  inv <- NULL  ## inv initialized to NULL for later use
  
    set <- function(y) { ## set allows for the mutation of data values and
                         ## takes an argument named y.
   
     x <<- y       ## the <<- operator assigns the input argument y to the 
                   ## x object in the parent environment.  
    
    inv <<- NULL ## assigns the value of NULL to the inv object in the 
                 ## parent environment thus clearing any value for inv 
                 ## cached by a prior execution of cacheSolve.
  }
  get <- function() x ## lexical scoping - x not defined within get()
                      ## retrieved from the parent environment of makeCacheMatrix
  
  setinverse <- function(solve) inv <<- solve ## As inv defined in parent 
                                              ## environment and will need to be
                                              ## accessed again after setinverse()
                                              ## executes, <<- used to assign input
                                              ## argument to inv in parent environment
                                              ## Solve function finds inverse of matrix
  
  getinverse <- function() inv ## same principle as for get function()x above 
  
  list(set = set, get = get,   ## Each fn assigned as an element of a list and is
       setinverse = setinverse,## returned to the parent environment.
       getinverse = getinverse)## Fn's assigned names which allows for use
                               ## of the extract operator $.
}

## cacheSolve computes the inverse of the special 
## "matrix" created by makeCacheMatrix. If the inverse 
## has already been calculated and the matrix remains unchanged, 
## cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) { ## starts with single arg x but allows for additional
                                 ## args through ...
## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse() ## getinverse() fn called on input object to retrieve inverse
  if(!is.null(inv)) {   ## checks to see if result is NULL. If not NULL...
    message("getting cached data")## this message along with cached inverse returned
    return(inv)                   ## to parent environment
  }
  data <- x$get()         ## If NULL, cacheSolve gets matrix from input object,  
  inv <- solve(data, ...) ## solves for its inverse,
  x$setinverse(inv)       ## uses setinverse on input object to set the inverse in the
                          ## in the input object
  inv                     ## returns value of inverse to parent environment 
}


