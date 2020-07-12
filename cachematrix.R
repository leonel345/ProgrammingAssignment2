# the matrix creator works adding the values of the matrix with 
#an integrator or in this way c (33,22,44,55,66) this  is the first argument
# the second is the number of rows and the third of columns
#if we leave the arguments of the function empty we create an array of 0 x 0
# for example try to put the arguments like this matriz <- (c(2,44,n ...),#rows,#columns)
makeCacheMatrix <- function(x = numeric(),j=0,k=0) {
  x<- matrix(x,nrow = j , ncol = k)
  i <- NULL
  #set the matrix just like you create it
  set <- function(y,w,e) {
    x <<- matrix(y,w,e)
    i <<- NULL
  }
  get <- function() x
  #ccalculate an inverse and then set it
  set_inversa <- function(inversa) i <<- inversa
  get_inversa <- function() i
  list(set = set, get = get, set_inversa = set_inversa,
       get_inversa = get_inversa)
}

###  we calculate the inverse and keep it in the cache
cacheSolve <- function(x) {
  i <- x$get_inversa()
  if(!is.null(i)) {
    message("obteniendo datos cache")
    return(i)
  }
  datos <- x$get()
  i <- solve(datos)
  x$set_inversa(i)
  return(i)
}