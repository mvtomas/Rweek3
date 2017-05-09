makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}


###Testes

##Teste 1

TestMatrix <- matrix(1:4,2,2)		
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)		
CacheMatrix$get()		
CacheMatrix$getinv()

cacheSolve(CacheMatrix)		
cacheSolve(CacheMatrix)

##Teste 2 - Matriz Singular
TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)		
TestMatrix		

CacheMatrix <- makeCacheMatrix(TestMatrix)		
CacheMatrix$get()		
CacheMatrix$getinv()		

cacheSolve(CacheMatrix)		
cacheSolve(CacheMatrix)
