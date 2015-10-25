# Following two functions are used to compute and cache the inverse of the matrix.

# makeCacheMatrix function creates a list with the following functions
# set = set the value of the matrix
# get = get the value of the matrix
# setinverse = set the inverse of the matrix
# getinverse = get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL		# Initialize inv with NULL
  
  set <- function(y) {	# Assigns value to x and inv in this environment
    x <<- y
    inv <<- NULL
  }
  
  get <- function()  {	# Gets the current value of matrix x
    x
  }  
  
  setinverse <- function(inverse){
    inv <<- inverse		# Assigns value to inv in this environment 
  }
  
  getinverse <- function(){	# Outputs value stored in inv
    inv
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)	# Creates list with the four specified functions
}



# cacheSolve function returns the inverse of the matrix. The function checks if
# the inverse is NULL or has been computed. If computed, it retrieves the result
# from the cache. If not computed, it computes the inverse and sets the value in
# the cache. It is assumed that the matrix supplied is invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()		# Assign inv with value of function getinverse() from list x
  
  if(!is.null(inv)) {			# check if inv is NULL. If not, inverse is available in cache.
    message("getting cached data.")
    return(inv)				# return value stored in inv
  }
  
  data <- x$get()				# else assign data with value of matrix
  inv <- solve(data)			# Compute inverse of matrix
  x$setinverse(inv)				# Set the value in cache inv 
  inv							# Output the inverse stored in inv
}

