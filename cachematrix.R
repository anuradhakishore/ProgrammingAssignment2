## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###This function creates a special "matrix" object that can store (cache) its inverse.It returns a list of functions to:
## - set a new matrix,
## - get the matrix,
## - set the inverse of the matrix,
## - get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function(){
    x
  }
  setInverse<- function(inverse){
    inv<<-inverse
  }
  getInverse<- function(){
    inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve: 
## This function computes the inverse of the matrix returned by makeCacheMatrix().
## If the inverse is already cached, it retrieves it from the cache.
## Otherwise, it calculates the inverse using solve(), caches it, and returns the result.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if (!is.null(inv)){
    message("Cache received")
    return (inv)
  }else{
    data<-x$get()
    inv<-solve(data)
    x$setInverse(inv)
    inv}
  }

