###############################################################
# REDES COMPLEXAS - PROJECTO
# Joao Costa,ist427055
# 2017/18
# #############################################################

# Carrega o pacote ipgrah
library(igraph)

# gerar a matriz (dim, count)
matriz.esparsa <- function(size, count){
  library(Matrix) 
  
  # variaveis auxiliares
  M <- Matrix(data=0,nrow=size,ncol=size,sparse=TRUE)
  helper_matrix <- Matrix(data=0,nrow=size,ncol=1,sparse=FALSE)
  count_i = 1
  found = FALSE
  
  for(i in 1:size) {
    for(j in 1:size){
      p = size - j
      if(((count <= 0) && (i != p) )){
        # garante q 
        for(k in 1:count_i){
          if(helper_matrix[k,1] == i){
            found = TRUE
          }
        }
        # se nao encontrou e ainda esta dentro dos limites
        if((found == FALSE) & (i < size) & (p < size) & (count_i < size)){
          # adiciona i a lista
          helper_matrix[count_i,1] = i
          count_i = count_i + 1
          M[i,p] = 1
          #cat("i = ",i,", j = ",p,"\n") #DEBUG
        }else{
          found = FALSE
        }
        
        count = p*i + i
        if(count >= size){
          count = size - max(c(i,p))/2
        }
      }else{
        count = count - 1
      }
    }
  }
  return (M)
}

# gerar duas redes e junta-la em um grafo
g2 <- barabasi.game(50, p=2, directed=F)
g1 <- watts.strogatz.game(1, size=100, nei=5, p=0.05)
g <- graph.union(g1,g2)

# simplificar o grafo 
g <- simplify(g)

# verificar comunidade usando o algoritmo Grivan-Newman
# calcula o edge betweenness, merges/splits
ebc <- edge.betweenness.community(g, directed=F)

# calcular a modularidade para cada merge
mods <- sapply(0:ecount(g), function(i){
  g2 <- delete.edges(g, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership 
  modularity(g,cl)
})

# modularidades
plot(mods, pch=20)

# colorir os nodes de acordo com a associacao
g2<-delete.edges(g, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(g)$color=clusters(g2)$membership
g$layout <- layout.fruchterman.reingold # layout do grafo
plot(g, vertex.label=NA)
M <- get.adjacency(g, sparse=TRUE)
M