## R Programming, Assignment 2: Caching the Inverse of a Matrix
##
## there are 4 functions below: two first are implemented as required in the assignment:
##
## 1) makeCacheMatrix       - creates a special "matrix" object that can cache its inverse
## 2) cacheSolve            - computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.
##
## as I'm not very happy of those 2 skeletons, I implemented an alternative function
## 3) makeSimpleCacheMatrix - calculats the inverse and chashes the result; 
##    there are no setters as not requred, only getters and the impl seems a bit more elegant
##
## the lasr function is for testing purposes
## 4) testSolve             - runs few test cases, positive and negative
##
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## this is requred to check if the matrix has changed
  
  dataMatrix <<- x
  invMatrix <<- NULL
  
  # set data
  setData <- function(y) {
    dataMatrix <- y
    invMatrix <- NULL
  }
  
  # get data
  getData <- function() dataMatrix
      
  #set inv matrix
  setSolve <- function(slv) invMatrix <<- slv
  
  #get inv matrix
  getSolve <- function() invMatrix
  
  #methods
  list(setData = setData, 
       getData = getData,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## calculate matrix inv and cache it
cacheSolve <- function(x, ...) {
  
  mi = tryCatch({
    x$getSolve()
  }, error = function(e) {
    stop("x is of wrong type, use makeCacheMatrix")
  })
  
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
    
  md = tryCatch({
    x$getData()
  },error = function(e) {
    stop("x is of wrong type, use makeCacheMatrix")
  })
  
  mi = tryCatch({
    solve(md)
  },error = function(e) {
    warning(e)
    stop("could not inverse matric")
  })
  
  x$setData(md)
  x$setSolve(mi)
  mi
  
}


## alternative implementation
## a) much simpler
## b) less prone to invalid call errors

makeSimpleCacheMatrix <- function(x = matrix()) {
  #md - matric to be inversed
  #mi - inverce matric
  dataMatrix <<- x
  invMatrix <<- tryCatch({
    solve(x)
  },error = function(e) {
    warning(e)
    stop("could not inverse matric")
  })
  
  # get data
  get <- function() dataMatrix
  
  #get inv matrix
  solve <- function() invMatrix
  
  #methods
  list(get = get, solve = solve)
}


## run tests
testSolve <- function(){
  
  m1 <- matrix(c(1,1,1,1,1,1,1,1,1), ncol = 3)
  m2 <- matrix(c(1,3,1,11,11,7,1,1,2), ncol = 3)
  
  #test invalid data - singular matrix
  m1Ext <- makeCacheMatrix(m1)
  m1Inv <- tryCatch({
    cacheSolve(m1Ext)
  }, error = function(e) {
    message(e)
    message("\n>>>>test1 ok: failed as expected\n")
  })
  
  
  #test invalid call
  m1Inv <- tryCatch({
    cacheSolve(m1)
  }, error = function(e) {
    message(e)
    message("\n>>>>test2 ok: failed as expected\n")
  })
  
  
  #test valid call
  m1Ext <- makeCacheMatrix(m2)
  
  #first call, caln solve
  m1Inv <- cacheSolve(m1Ext)
  message("\n>>>>test3 ok: first call, inv is calculated\n")
  #second call, return from cache - see message
  m1Inv <- cacheSolve(m1Ext)
  message("\n>>>>test4 ok: (if there is a message `getting cached data`)\n")
  
  
  #test for alternative imp 
  #test invalid data - singular matrix
  m1Inv <- tryCatch({
   makeSimpleCacheMatrix(m1)
  }, error = function(e) {
   message(e)
    message("\n>>>>test5 ok: failed as expected\n")
  })
  
  #test valid 
  m1Inv <- makeSimpleCacheMatrix(m2)
  m1Inv$solve()  
  m1Inv$solve()  
  m1Inv$solve()  
  message("\n>>>>test6 ok\n")
}
