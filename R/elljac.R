"elljac" <- function(u, m, give=FALSE, strict=TRUE){
  jj <- process.2.args(u, m)
  u.vec <- jj$arg1
  m.vec <- jj$arg2
  attr <- jj$attr

  jj <- .C("elljac_e",
           as.double(u.vec),
           as.double(m.vec),
           as.integer(length(u.vec)),
           sn=as.double(u.vec),
           cn=as.double(u.vec),
           dn=as.double(u.vec),
           status=seq(along=u.vec),
           PACKAGE="gsl"
           )

  sn <- jj$sn
  cn <- jj$cn
  dn <- jj$dn
  attributes(sn) <- attr
  attributes(cn) <- attr
  attributes(dn) <- attr

  status <- jj$status
  attributes(status) <- attr
  
  if(strict){
    sn <- strictify(sn,status)
    cn <- strictify(cn,status)
    dn <- strictify(dn,status)
  }
  
  if(give){
      return(list(list(sn=sn,cn=cn,dn=dn),status=status))
  } else {
    return(list(sn=sn,cn=cn,dn=dn))
  }
}  

