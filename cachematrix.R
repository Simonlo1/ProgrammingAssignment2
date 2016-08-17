## makeCacheMatrix and cacheSolve are used together to optimize the calculation
## of inverted matrix by caching previously calculated result.
##
## makeCacheMatrix create a special matrix object (list):
##      sm <- makeCacheMatrix(x) : Return a special matrix object initialized with x
##      sm$set(x) : Set storedd matrix to x and clear cached inverted result
##      sm$get() : Return the original stored matrix object
##      sm$setinverse(ix) : Store a calculated inverted matrix. Used by cacheSolve
##      sm$getinverse() : return the stored inverted matrix. Used by cacheSolve
##
## cacheSolve return the inverted matrix by calculating it or, if
##    it as already been calculated, using previously calculated result:
##      cacheSolve(sm) : Return the inverted matrix of sm
## 
## Usage :
## 1. Initialise special matrix object by using sm <- makeCacheMatrix(m) where
##    m is a square invertible matrix.
##
## 2. cacheSolve(sm) will return the inverted matrix by calculating it or, if
##    it as already neen calculated, the previously calculated result.

## makeCacheMatrix return a special matrix object (list) to store the matrix
## and its cached inverse is it as already been calculated.

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve take a special matrix object created by makeCacheMatrix and
## calculate or return the previsously calculated matrix inverse.

cacheSolve<-function(x,...){
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
        
