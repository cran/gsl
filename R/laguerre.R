"laguerre_1" <- function(a, x, give=FALSE, strict=TRUE){
  jj <- process.2.args(a,x)
  a.vec <- jj$arg1
  x.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("laguerre_1",
           as.double(a.vec),
           as.double(x.vec),
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
"laguerre_2" <- function(a, x, give=FALSE, strict=TRUE){
  jj <- process.2.args(a,x)
  a.vec <- jj$arg1
  x.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("laguerre_2",
           as.double(a.vec),
           as.double(x.vec),
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
"laguerre_3" <- function(a, x, give=FALSE, strict=TRUE){
  jj <- process.2.args(a,x)
  a.vec <- jj$arg1
  x.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("laguerre_3",
           as.double(a.vec),
           as.double(x.vec),
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
"laguerre_n" <- function(n, a, x, give=FALSE, strict=TRUE){
  jj <- process.3.args(n,a,x)
  n.vec <- jj$arg1
  a.vec <- jj$arg2
  x.vec <- jj$arg3
  attr <- jj$attr

  jj <- .C("laguerre_n",
           as.integer(n.vec),
           as.double(a.vec),
           as.double(x.vec),
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
