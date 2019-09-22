opt.cut = function(prf, pr){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, prf@x.values, prf@y.values, pr@cutoffs)
}

print(opt.cut(prf,pr))


cut.ind=mapply(FUN=function(prf,pr,p){
  d=(prf-0)^2+(pr-1)^2
  ind=which(d==min(d))
  c(sensitivity=pr[[ind]],specificity=1-prf[[ind]],cutoff=p[[ind]])
}, prf@x.values, prf@y.values, pr@cutoffs)



?mapply
class(cut.ind)
cut.ind
