library(data.table)
library(rlang)

slim_ggplot <- function(p) {
  vars_of <- function(mapping) {
    if (is.null(mapping)) return(character())
    unlist(lapply(mapping, function(q) all.vars(quo_get_expr(as_quosure(q)))))
  }
  used <- vars_of(p$mapping)
  for (ly in p$layers) used <- c(used, vars_of(ly$mapping))
  fp <- p$facet$params
  for (f in c(fp$facets, fp$rows, fp$cols)) used <- c(used, all.vars(quo_get_expr(as_quosure(f))))
  used <- unique(used)
  
  shrink <- function(d) {
    if (is.null(d) || !is.data.frame(d)) return(d)          # waiver()/inherited -> leave alone
    keep <- intersect(used, names(d))
    if (!length(keep)) return(d)
    unique(as.data.table(d)[, ..keep])                       # drop unused cols, then de-duplicate rows
  }
  p$data   <- shrink(p$data)
  p$layers <- lapply(p$layers, function(ly) { ly$data <- shrink(ly$data); ly })
  p
}