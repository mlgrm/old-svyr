random.nested.table <- function(force.list=TRUE, max.el=7, plist=.1){
  if(force.list) dolist=TRUE else dolist=rbinom(1,1,plist)
  if(dolist)
    sapply(letters[1:sample(2:max.el,1)], function(l)
      random.nested.table(force.list=FALSE, max.el=max.el, plist=plist),
      simplify=FALSE) else {
        dim=sample(2:max.el,2)
        matrix(rnorm(dim[1]*dim[2]),dim[1],dim[2])
      }
}
