"pow_int" <- function(x, n, give=FALSE, strict=TRUE){

  jj <- process.2.args(x,n)
  x.vec <- jj$arg1
  n.vec <- jj$arg2
  attr <- jj$attr

  
  jj <- .C("pow_int",
           as.double(x.vec),
           as.integer(n.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attr
  err <- jj$err
  status <- jj$status
  attributes(err) <- attr
  attributes(status) <- attr

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}  

