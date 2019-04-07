## we use this funcation to calculate inverse matrix of cache 

## create special matrix object can cache it to inverse 

makeCacheMatrix <- function(x = matrix()) {
  q <- NULL                                           #inverese property
  
  set <- function(matrix){                            #method to set matrix   
    z <<- matrix
    q <<- NULL
  }
  
  get <- function(){                                  #method to get matrix
    z                                                 #return the matrix
  }
  
  setinverse <- function(){                           #set the inverse of matrix
    q <<- inverse                                     
  }
  
  getinverse <- function(){                           #get the inverse of matrix
    q
  }
  
  list(set = set, get = get,                          #list of method
       setInverse = setInverse,
       getInverse = getInverse)

}


##  Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  if(!is.null(n)){                              #if inverse is already set , it will return it
    message("getting cached data")
    return(n)
  }
  data <-x$get()                                #get the matrix 
  
  n <- solve(data) %*% data                     #calculate the inverse
  
  x$setInverse(n)                               #set the inverse into your object
  
  n
}
