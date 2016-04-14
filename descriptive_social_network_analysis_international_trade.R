setwd("/Users/sergej/Google Drive/QASS_Master/Network-Analysis-Week2/Assignment/Assignment1")
rm=ls()
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
# Note that the output of graph.coreness refers simply to the *degree*
# of the k-core, not to the k-core itself; thus, two vertices both with 
# coreness of 3 may not be connected at all and thus may be in separate
# k-cores. 
#
# One way to get a sense of the actual k-core structure is to simply 
# plot the graph and color-code by k-core:
make_k_core_plot <- function (g) {
  lay1 <- layout_with_fr(g)
  plot(g, 
       vertex.color = graph.coreness(g), 
       layout=lay1, 
       edge.arrow.size = .5)
} 
cum.distr.plot<-function(g,attrib ){
  df<-get.data.frame(g,what=c("vertices"))
  namex <- deparse(substitute(attrib))
  call<-substitute(dplyr::select(df, name, attrib))
  v<-eval(call)
  
  v<-sort(v)
  v<-as.data.frame(v)
  call.3<-substitute(plyr::arrange(df,order(attrib)))
  df<-eval(call.3)
  namexs<-c("COU",deparse(substitute(attrib)))
  names(v)<-namexs
  print(v)
  v$cum.rel.freq<-cumsum(v[,2])/sum(v[,2])
  
  plot( x=v[,1], y=v[,2], pch=19, cex=1.2,  col="orange", 
        xlab=namex, ylab="Cumulative Frequency", main="Cummulative distribution")
  return(v)
}
detachAllPackages()
lapply(c("sna", "ggplot2","intergraph", "GGally", "igraph", "network", "plyr","dplyr","ocean"), 
       require, character.only=T)
load("edgelist_weights.Rdata")
load("verticies_additional.Rdata")

exgr.2005.manufac.graph<-graph.data.frame(exports.2005.manufacturing[,c(1:2,9)], vertices=exports.2005.manufacturing.actors)


#exclude self loops, result is a simple graph
exgr.2005.manufac.graph<- simplify(exgr.2005.manufac.graph, remove.loops=TRUE, remove.multiple = F)
#diameter, density, mean shortest path
#longest shortest path2
exgr.2005.manufac.graph$diameter<-diameter(exgr.2005.manufac.graph, directed = TRUE, unconnected = TRUE, weights = NULL)
#very dense 0.7
exgr.2005.manufac.graph$density<-edge_density(exgr.2005.manufac.graph, loops=F)
#low mean distance
exgr.2005.manufac.graph$mean_distance<-mean_distance(exgr.2005.manufac.graph, directed=T)
#degree distribution #measure of influence

V(exgr.2005.manufac.graph)$deg.in <- degree(exgr.2005.manufac.graph, mode="in")

V(exgr.2005.manufac.graph)$deg.out <- degree(exgr.2005.manufac.graph, mode="out")



edge_attr_names(exgr.2005.manufac.graph)

deg.dist <- degree_distribution(exgr.2005.manufac.graph, cumulative=T, mode="all")
plot( x=0:max(degree(exgr.2005.manufac.graph, mode="all")), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

deg.dist.in <- degree_distribution(exgr.2005.manufac.graph, cumulative=T, mode="in")
deg.dist.out<-degree_distribution(exgr.2005.manufac.graph, cumulative=T, mode="out")
#heterogeneity implied by the degree distribution
tikz(file="degree_distr.tex")
par(mfrow=c(1,2))

plot( x=0:max(V(exgr.2005.manufac.graph)$deg.in), y=1-deg.dist.in , pch=19, cex=1.2, col="orange", 
      xlab="In Degree", ylab="Cumulative Frequency")

plot( x=0:max(V(exgr.2005.manufac.graph)$deg.out), y=1-deg.dist.out, pch=19, cex=1.2,  col="orange", 
      xlab="Out Degree", ylab="Cumulative Frequency")
dev.off()
par(mfrow=c(1,1))

#value is mean of two countries in trading partnership
# closeness centrality 
V(exgr.2005.manufac.graph)$closeness.centr.in<-closeness(exgr.2005.manufac.graph, mode="in", weights=NA ) 
V(exgr.2005.manufac.graph)$closeness.centr.out<-closeness(exgr.2005.manufac.graph, mode="out", weights=NA ) 
#0.28 implies that network quite far away from being domination by single actor, but also no completly equal 
exgr.2005.manufac.graph$Network.centralization.in<-centr_clo(exgr.2005.manufac.graph, mode="in", normalized=T)$centralization
exgr.2005.manufac.graph$Network.centralization.out<-centr_clo(exgr.2005.manufac.graph, mode="out", normalized=T)$centralization


#hubscores
V(exgr.2005.manufac.graph)$hub.s <- hub_score(exgr.2005.manufac.graph, weights=NA)$vector
V(exgr.2005.manufac.graph)$authr.s <- authority_score(exgr.2005.manufac.graph, weights=NA)$vector


#transitivity clustering
exgr.2005.manufac.graph$trans.global<-transitivity(exgr.2005.manufac.graph,type=c("global"))

V(exgr.2005.manufac.graph)$trans.local<-transitivity(exgr.2005.manufac.graph,type=c("local"))
cum.distr.plot(g=exgr.2005.manufac.graph, attrib=trans.local)
set_graph_attr(exgr.2005.manufac.graph,trans.global)
exgr.2005.network<-intergraph::as.network((exgr.2005.manufac.graph))

par(mfrow(c(1,2)))
plot(exgr.2005.manufac.graph, vertex.size=V(exgr.2005.manufac.graph)$hs*10, main="Hubs", layout=layout_with_fr(exgr.2005.manufac.graph),vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5)
plot(exgr.2005.manufac.graph, vertex.size=V(exgr.2005.manufac.graph)$authr.s*15, main="Hubs", layout=layout_with_fr(exgr.2005.manufac.graph),vertex.label.dist = .6, vertex.label.cex = 0.5, edge.width = 0.1)

l.nice<-layout_nicely(exgr.2005.manufac.graph) 



exgr.2005.manufac.graph.undir<-shortest_paths(exgr.2005.manufac.graph,BEL)
exgr.2005.manufac.graph.undir<-as.undirected(exgr.2005.manufac.graph, mode="mutual")
#community detection 
cluster.exgr.2005<-cluster_louvain(exgr.2005.manufac.graph.undir )
members<-membership(cluster.exgr.2005)
tikz(file = "communities.tex")
plot(exgr.2005.manufac.graph, vertex.color =members, vertex.size=0.05*deg.out , layout=layout_with_fr(exgr.2005.manufac.graph, repulserad = vcount(g)^2.8, 
area = vcount(g)^2.3 ),vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5)
dev.off()
lay.2<-layout_with_lgl(exgr.2005.manufac.graph, area=vcount(exgr.2005.manufac.graph)^2.5,maxdelta=vcount(exgr.2005.manufac.graph)^4)

     adj.exgr.2005<-as.matrix()
     exgr.2005.manufac.graph.net<-asNetwork(exgr.2005.manufac.graph)
     # components
     weak.comp<-clusters(exgr.2005.manufac.graph, mode=c("weak"))
     strong.comp<-clusters(exgr.2005.manufac.graph, "strong")
     #biconected no articulation points
     biconnect<-biconnected.components(exgr.2005.manufac.graph.undir)
     #coreness
     exgr.2005.manufac.graph$coreness<-coreness(exgr.2005.manufac.graph, mode="all")
     coreness <- graph.coreness(exgr.2005.manufac.graph)
     coreness
     
    
     make_k_core_plot(exgr.2005.manufac.graph)
     cores<-as.data.frame(graph.coreness(exgr.2005.manufac.graph))
     cores$COU<-rownames(cores)
     names(cores)<-c("k.core","COU")
     
     cores<-arrange(cores,(k.core))
     cores$cum.rel.freq<-cumsum(cores$k.core)/sum(cores$k.core)
     # Code based on Jordi Casas-Roma https://jcasasr.wordpress.com/2015/02/03/plotting-the-coreness-of-a-network-with-r-and-igraph/
     CorenessLayout <- function(g) {
       coreness <- graph.coreness(g);
       xy <- array(NA, dim=c(length(coreness), 2));
       
       shells <- sort(unique(coreness));
       for(shell in shells) {
         v <- 1 - ((shell-1) / max(shells));
         nodes_in_shell <- sum(coreness==shell);
         angles <- seq(0,360,(360/nodes_in_shell));
         angles <- angles[-length(angles)]; # remove last element
         xy[coreness==shell, 1] <- sin(angles) * v;
         xy[coreness==shell, 2] <- cos(angles) * v;
       }
       return(xy);
     }
     # compute coreness
     coreness <- graph.coreness(exgr.2005.manufac.graph);
     # assign colors
     require(ocean)
     colbar <- jet.colors(max(coreness))
    
     # create layout
     ll <- CorenessLayout(exgr.2005.manufac.graph);
     #try visualize k-core
     plot(exgr.2005.manufac.graph, layout=ll, vertex.size=5, vertex.color=colbar[coreness], vertex.frame.color=colbar[coreness], main='Coreness',  edge.width = 1,
          edge.arrow.size = 0.1,vertex.label.dist = 0.5, vertex.label.cex = 0.7);
    tikz(file="k-core.tex")
      plot( x=cores$k.core, y=cores$cum.rel.freq, pch=19, cex=1.2,  col="orange", 
           xlab="k-core", ylab="Cumulative Frequency", main="Cummulative distribution of k-cores")
dev.off()
      #betweness     
V(exgr.2005.manufac.graph)$between.centr<-betweenness(exgr.2005.manufac.graph, directed=T, normalized=F)
  exgr.2005.manufac.graph$betweness.centralization<-centr_betw(exgr.2005.manufac.graph, directed=T, normalized=F)$centralization

 influence.trade.df<- data.frame(V(exgr.2005.manufac.graph)$name, V(exgr.2005.manufac.graph)$deg.in, V(exgr.2005.manufac.graph)$deg.out, V(exgr.2005.manufac.graph)$closeness.centr.in, V(exgr.2005.manufac.graph)$closeness.centr.out, 
                                 V(exgr.2005.manufac.graph)$between.centr, V(exgr.2005.manufac.graph)$hub.s, V(exgr.2005.manufac.graph)$authr.s)
 influence.trade.df$rel.degree.in<-influence.trade.df$deg.in/(143-1)
 influence.trade.df$rel.degree.out<-influence.trade.df$deg.out/(143-1)
 names(influence.trade.df)<-gsub("V.exgr.2005.manufac.graph..","",names(influence.trade.df))
 cor(influence.trade.df$between.centr, influence.trade.df$authr.s,method=c("spearman"))

 graph.export<-graph.data.frame(exports.2005.manufacturing[,c(1:2,9)], vertices=exports.2005.manufacturing.actors)
 E(graph.export)$weight<-E(graph.export)$value
 V(graph.export)$id<-V(graph.export)$name
 closeness.centr.out<-as.data.frame(V(exgr.2005.manufac.graph)$closeness.centr.out)
 df<-get.data.frame(exgr.2005.manufac.graph,what=c("vertices"))

 
 
 closeness.centr.out
 names<-as.character(closeness.centr.out)
 concept<-vertex_attr(g, names)
   names<-x
names(df[,1])<-"names"

 closeness.centr.out$COU<-V(exgr.2005.manufac.graph)$name
 names(closeness.centr.out)<-c("out.closeness","COU")
 
 closeness.centr.out<-arrange(closeness.centr.out,(out.closeness))
 closeness.centr.out$cum.rel.freq<-cumsum(closeness.centr.out$out.closeness)/sum(closeness.centr.out$out.closeness)
 #exclude self loops, result is a simple graph
 exgr.2005.manufac.graph<- simplify(exgr.2005.manufac.graph, remove.loops=TRUE, remove.multiple = F)
 write.graph(graph.export, file="exgr_manuf.net", format=c("pajek"))
