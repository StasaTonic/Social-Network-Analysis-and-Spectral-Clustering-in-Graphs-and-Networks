install.packages("igraph")

library(igraph)
gnp_Graph <- sample_gnp(20, 1, directed = FALSE, loops = FALSE)
plot(gnp_Graph)

Star_Graph <- make_star(10, center = 1)
plot(Star_Graph)

Ring_Graph <- make_ring(12, directed = FALSE, mutual = FALSE, circular = FALSE)
plot(Ring_Graph)


install.packages("readr")
library(readr)

actors <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Actors.csv")
movies <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Movies.csv")

actors
movies

actorNetwork <- graph_from_data_frame(d=movies, vertices = actors, directed = FALSE)
plot(actorNetwork)

E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", 
                                ifelse(E(actorNetwork)$Movie == "Apollo 13", "black",
                                       "orange"))
                                       
plot(actorNetwork)

V(actorNetwork)$color <- ifelse(V(actorNetwork)$BestActorActress == "Winner", "gold",
                                ifelse(V(actorNetwork)$BestActorActress == "Nominated","grey",
                                       "lightblue"))
                                       
plot(actorNetwork)


plot(actorNetwork, vertex.frame.color="white")

legend("bottomright", c("Winner","Nominee", "Not Nominated"), pch=21,
       col="#777777", pt.bg=c("gold","grey","lightblue"), pt.cex=2, cex=.8)


legend("topleft", c("Forest Gump","Apollo 13", "The Rock"), 
       col=c("green","black","orange"), lty=1, cex=.8)


degree(actorNetwork, mode="all")

betweenness(actorNetwork, directed=F, weights=NA, normalized = T)

closeness(actorNetwork, mode="all", weights=NA, normalized=T)

eigen_centrality(actorNetwork)

Eig <- evcent(actorNetwork)$vector
Eig

centralities <- cbind(Degree, Eig, Hub, Authority, Closeness, Reach_2, Reach_3, Betweenness)

bet <- betweenness(actorNetwork, directed=F, weights=NA, normalized = T)
plot(actorNetwork, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )
 
install.packages("mtcars")
library(MASS)
Boston

X <- data.matrix(Boston)
X

spectral_clustering <- function(X, # matrix of data points
                                nn = 10, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep

mutual_knn_graph <- function(X, nn = 10)

D <- as.matrix( dist(X) )    

knn_mat <- matrix(0,
                  nrow = nrow(X),
                  ncol = nrow(X))

for (i in 1: nrow(X)) {
  neighbor_index <- order(D[i,])[2:(nn + 1)]
  knn_mat[i,][neighbor_index] <- 1 
}




~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  
#Instaliramo paket mlbench
install.packages("mlbench")
library(mlbench)

#Formiramo skup podataka
set.seed(111)
obj <- mlbench.spirals(100,1,0.025)
my.data <-  4 * obj$x
plot(my.data)

#Racunamo matricu slicnosti S Gausovom metodom jezgara
s <- function(x1, x2, alpha=1) {
  exp(- alpha * norm(as.matrix(x1-x2), type="F"))
}

make.similarity <- function(my.data, similarity) {
  N <- nrow(my.data)
  S <- matrix(rep(NA,N^2), ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      S[i,j] <- similarity(my.data[i,], my.data[j,])
    }
  }
  S
}

S <- make.similarity(my.data, s)
S[1:8,1:8]

#Pravimo matricu afiniteta A na osnovu S, koristi se knn algoritam
make.affinity <- function(S, n.neighboors=2) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) {
      #povezati samo sa tackama sa vecom slicnosti 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] #simetricna matrica
      }
    }
  }
  A  
}

A <- make.affinity(S, 3)  
A[1:8,1:8]

#pravimo dijagonalnu matricu
D <- diag(apply(A, 1, sum)) 
D[1:8,1:8]

#Racunamo nenormalizovani Laplasovu matricu
U <- D - A
round(U[1:12,1:12],1)

#Funkcija za racunanje stepena matrice
"%^%" <- function(M, power)
  with(eigen(M), vectors %*% (values^power * solve(vectors)))

#Trazimo k najmanjih sopstvenih vektora
k   <- 2
evL <- eigen(U, symmetric=TRUE)
Z   <- evL$vectors[,(ncol(evL$vectors)-k+1):ncol(evL$vectors)]
#i/ti red u Z predstavlja transformaciju od Xi

#Proveravamo da li su tacke dobro razdvojene
plot(Z, col=obj$classes, pch=20) 

#kmeans clustering
library(stats)
km <- kmeans(Z, centers=k, nstart=5)
plot(my.data, col=km$cluster)

#Ako ne znamo koliko ima klastera,spektar sopstvenih vrednosti ima razdor koji nam daje vrednost od k
signif(evL$values,2)

plot(1:10, rev(evL$values)[1:10], log="y")
abline(v=2.25, col="red", lty=2)


#Ovo je vec ugradeno u R/u u okviru kernlab paketa
install.packages("kernlab")
library(kernlab)

sc <- specc(my.data, centers=2)
plot(my.data, col=sc, pch=4) 
points(my.data, col=obj$classes, pch=5) 
