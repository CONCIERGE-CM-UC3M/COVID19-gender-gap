source("iu_clean.R")

get_main <- function(data, del, all) {
  data[!subcategory %in% del][
    , .(main = all[all %in% subcategory][1]), by=id][
      , .N, by=main][order(N, decreasing=TRUE)]
}

psyarxiv <- articles[repository == "psyarxiv"]
psyarxiv.v <- psyarxiv[, .(weight=.N), by=subcategory][order(weight, decreasing=TRUE)]
psyarxiv.v

get_main(psyarxiv, NULL, psyarxiv.v$subcategory)

socarxiv <- articles[repository == "socarxiv"]
socarxiv.v <- socarxiv[, .(weight=.N), by=subcategory][order(weight, decreasing=TRUE)]
socarxiv.v

get_main(socarxiv, NULL, socarxiv.v$subcategory)

o <- socarxiv.v$subcategory
del <- c("Social and Behavioral Sciences", "Sociology", "Arts and Humanities", "Psychology", "Economics")
last <- get_main(socarxiv, character(0), o)$main
cat("Inicial :\n"); print(last); cat("\n")
for (i in seq_along(del)) {
  sel <- get_main(socarxiv, del[1:i], o)
  cat(del[i], ":\n")
  print(setdiff(sel$main, last)); cat("\n")
  last <- sel$main
}

sum(.Last.value$N)/length(unique(socarxiv$id))

################################################################################

psyarxiv <- articles[repository == "psyarxiv"]
psyarxiv.v <- psyarxiv[, .(weight=.N), by=subcategory][order(weight, decreasing=TRUE)]
psyarxiv.v

o <- psyarxiv.v$subcategory
del <- c("Social and Behavioral Sciences")
psyarxiv[
  !subcategory %in% del][
    , .(main = o[o %in% subcategory][1]), by=id][
      , .N, by=main][order(N, decreasing=TRUE)]
sum(.Last.value$N)/length(unique(psyarxiv$id))

################################################################################

total <- length(unique(socarxiv$id))
keep <- socarxiv.v[c(2, 4:20)]$subcategory
100*(length(unique(socarxiv[subcategory %in% keep]$id)))/total

plot(10:100, sapply(10:100, function(i) {
  keep <- socarxiv.v[c(2, 4:i)]$subcategory
  100*(length(unique(socarxiv[subcategory %in% keep]$id)))/total
}))

################################################################################

pairs <- as.data.frame(t(combn(unique(socarxiv$subcategory), 2)))
socarxiv.e <- merge(socarxiv, pairs, by.x="subcategory", by.y="V1", allow.cartesian=TRUE)
socarxiv.e[, keep := V2 %in% subcategory, by=id]
socarxiv.e <- socarxiv.e[keep==TRUE][, .(weight=.N), by=.(subcategory, V2)]

library(igraph)
g <- graph_from_data_frame(socarxiv.e, directed=FALSE, socarxiv.v)
#g.gexf <- rgexf::igraph.to.gexf(g)
#writeLines(capture.output(g.gexf), "socarxiv_subcategories.gexf")
cl <- cluster_louvain(g)
sizes(cl)
sapply(communities(cl), function(i) sum(V(g)[i]$weight))
V(g)$membership <- membership(cl)
ee <- as.data.frame(ends(g, E(g)))
ee <- cbind(ee, memV1=V(g)[ee[,1]]$membership, memV2=V(g)[ee[,2]]$membership)
g <- delete_edges(g, with(ee, which(memV2 != memV1)))

V(g)$degree <- degree(g)
vx <- as.data.table(as_data_frame(g, "vertices"))
#View(vx[order(membership, degree, decreasing=TRUE)])
vx.main <- vx[order(membership, degree, decreasing=TRUE), head(.SD, 1), by=membership]
g <- delete_vertices(g, vx.main[order(degree, decreasing=TRUE)]$name[1:2])

cl <- cluster_louvain(g)
sizes(cl)
sapply(communities(cl), function(i) sum(V(g)[i]$weight))
V(g)$membership <- membership(cl)

V(g)$degree <- degree(g)
vx <- as.data.table(as_data_frame(g, "vertices"))
View(vx[order(membership, degree, decreasing=TRUE)])
