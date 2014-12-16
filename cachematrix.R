## Put simply, makeCacheMatrix assigns to a storage vector several functions which store the value of a matrix vector,  
#  allows you to retrieve that value, stores the inverse of that matrix vector (when calculated by the cacheSolve 
#  function), and allows you to retrieve that inverse.  cacheSolve returns the inverse of the matrix you call, first 
#  by looking to see if it is already been calculated and stored previously and if not, calculating the matrix and 
#  storing it in the storage vector for later retrieval when cacheSolve is called again.  
#  
# The rules: 
#   You have to call makeCacheMatrix with a vector of class matrix. 
#   You have to call makeCacheMatrix before you can call cacheSolve




## Creates a storage vector for matrix x and its inverse 

makeCacheMatrix <- function(x = matrix()) {#makeCacheMatrix is a function that takes a matrix vector
  inverse <- NULL                          #this sets the vector inverse to empty (whenever the function is called)
  set <- function (y = matrix()) {         #this creates the method set (x.set) which takes a matrix vector y
    x <<- y                                #this makes the vector of matrix x the vector of matrix y
    inverse <<- NULL                       #this resets the inverse to empty in case it previously had a value for old x
  }                                        #end of the x$set method
  get <- function() {                      #this creates the method get, called as x$get
    x                                      #all this function does is return x (the original matrix)                                   
  }                                        #end of the x$get method
  setinverse <- function(calculated.inverse){#creates the method x$setinverse which takes the calculated inverse 
    inverse <<- calculated.inverse         #assigns inverse to be the calculated inverse from cacheSolve
  }                                        # end of the x$setinverse method
  getinverse <- function() {               #this creates the method x$getinverse
    inverse                                #all this function does is return the inverse
  }                                        #end of the x$getinverse method
  list(set = set,                          #creates a list that allows the above methods to be indexed using the $
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
  
  
  

}


## Returns the inverse of x.  Either a) returns the chached inverse or b) calculates the inverse, chaches it,
## then returns the inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse                #looks up the inverse from the storage vector
  if(!is.null(inverse)) {                #asks if inverse has a value, if true then
    message("getting chached data")      #first generates a message in the prompt 
    return (inverse)                     #then returns the value of the matrix x and exits the function
  }  
  data <- x$get()                        #assigns to data the matrix x from the storage vector
  inverse <- solve(data, ...)            #calculates the inverse of matrix x
  x$setinverse (inverse)                 #stores the newly calculated inverse
  inverse                                #returns the inverse of the matrix x
}
