##### WEEK 3 HW ######

##### Eample with vector means ######
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
} 

vec <- makeVector(1:3)
str(vec)
cachemean(vec)
vec$get()
vec$getmean()
vec$set(1:4)

test.s <- function(mean) m <<- mean
test.s(1:4)
m

##### ASSIGNMENT invert a matrix ######
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(solve) inver <<- solve
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}

x<-matrix(1:4, nrow = 2, ncol = 2)
x
solve(x)
sol.inver <- makeCacheMatrix(x)
cacheSolve(sol.inver)

