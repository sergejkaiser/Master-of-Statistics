setwd("/Users/sergej/Google Drive/QASS_Master/Network-Analysis-Week2")
#Structure 
#1. Descriptives of network 
#2. Gender 
# 2.1 Descriptives (denity)
# 2.2 
#1.a. What are the main characteristics of the affective network at the two observations? Are they similar/different?
#Descriptives of networks, dissimilarity with descriptives, qarp,   


# count the different dyad types
?dyad.census

affective_w1<-as.matrix(read.csv("RECENS_data/7400_affective_w1.csv",  header=TRUE, row.names=1, sep=","))
affective_w2<-as.matrix(read.csv("RECENS_data/7400_affective_w2.csv",  header=TRUE, row.names=1, sep=","))
sex<-as.matrix(read.csv("RECENS_data/7400_sex.csv",  header=TRUE, row.names=1, sep=","))
drink<-as.matrix(read.csv("RECENS_data/7400_drink.csv",  header=TRUE, row.names=1, sep=","))

# 4. Explore the data

# here are some questions for guidance (you can do more)
# it's a good idea to put the answers into objects - you will have them later


# How many students are in the classroom?
# (as many as rows in your data objects)
dim(affective_w1) 
dim(affective_w2)
class_size <-  nrow(affective_w1)

# How many boys and girls?
# (if only you could make a table with the frequencies and put it in an object...)

#1 5 2 25

gender_comp <- table(sex)
#orginial
affective_w1.orig<-affective_w1
affective_w2.orig<-affective_w2
affective_w2<-affective_w2.orig
affective_w1<-affective_w1.orig
sex.orig<-sex

# How much missing data is in the affective network?
# (earlier we did this by combining two functios: is.na and sum)
# you only need to count the number of NAs in the matrix
affect_miss_w1 <-  sum(is.na(affective_w1))
affect_miss_w2 <-  sum(is.na(affective_w2))
# run the next line to divide it by the number of possible ties
# and get the proportion of missing values
affect_miss_w1 <- affect_miss_w1 / ( nrow(affective_w1) * (ncol(affective_w1) - 1) )
affect_miss_w2 <- affect_miss_w2 / ( nrow(affective_w2) * (ncol(affective_w2) - 1) )

# W1 0.1678161 Missing values
#W2 0.2413793 Missing values
# (advanced users: check diag(affective_w1) and think about how we could correct the result)
diag(affective_w1) <-0
diag(affective_w2) <-0
affect_miss_w1 <- affect_miss_w1 / ( nrow(affective_w1) * (ncol(affective_w1) - 1) )
affect_miss_w2 <- affect_miss_w2 / ( nrow(affective_w2) * (ncol(affective_w2) - 1) )
#1 --- 0.0001928921 nearly complete 2--- 0.0002774475
# How many students were absent at the time of the data collection?
# (these are the people whose rows in the affective matrix only contains NAs)

#create new object which countrs the number of NA in a row
#rows with 29 missings are missing

row<-apply(affective_w1,1,function(affective_w1) sum(is.na(affective_w1)))
row.missings<-sum(row==class_size-1)

row.2<-apply(affective_w2,1,function(affective_w2) sum(is.na(affective_w2)))
row.2.missings<-sum(row.2==class_size-1)
#w1: 4 missing w2:6 missing
absent_w1 <-sum(row==29)
absent_w2 <-sum(row.2==29)
names(row[row==29])
names(row.2[row.2==29])




# for the following questions, load the package sna if its not yet loaded
library(sna)

affective_w1[affective_w1 %in% c(1,-1,-2)] <- 0
affective_w1[affective_w1==2] <-1
affective_w2[affective_w2 %in% c(1,-1,-2)] <- 0
affective_w2[affective_w2==2] <-1
# in the sna package, there are functions four counting different structural
# configurations in a network, for example: dyads, triads, cycles, paths
# here we try a few of these functions
# How dense is the affective network?
aff_dens_w1 <- sna::gden(affective_w1) #0.1525
aff_dens_w2 <- sna::gden(affective_w2) # 0.1555
# How many of the dyads are reciprocated?
# (remember to specify the arguments correctly, look at ?grecip if you forgot how to)
aff_rec_w1 <-sna::grecip(affective_w1, measure="dyadic.nonnull") #0.52
aff_rec_w2 <-grecip(affective_w2, measure="dyadic.nonnull") #0.4
#decreased
# How do the in- and outdegree distributions look like in the classroom?
# (plot them based on the previous scipt)
# first count the degrees
aff_ind_w1 <- degree(affective_w1, cmode="indegree") # indegrees
aff_outd_w1 <- degree(affective_w1, cmode="outdegree") # outdegrees
aff_ind_w2 <- degree(affective_w2, cmode="indegree") # indegrees
aff_outd_w2 <- degree(affective_w2, cmode="outdegree") # outdegrees
# then plot them using histograms (they don't have to look nice and don't have to be
# combined in a single figure; but they can be)
par(mfrow=c(2,2))
hist(aff_ind_w1 ,main = "Indegree Wave1", ylim=c(0,12), xlim=c(0,10),     xlab ="# Indegees")
hist(aff_outd_w1,main = "Outdegree Wave 1", ylim=c(0,12), xlim=c(0,12),     xlab ="# Outdegees")
hist(aff_ind_w2,main = "Indegree Wave 2", ylim=c(0,12), xlim=c(0,10),     xlab ="# Indegees")
hist(aff_outd_w2,main = "Outdegree Wave 2", ylim=c(0,12), xlim=c(0,12),     xlab ="# Outdegees")

#evolution indegree more evenly till indegree 6 
#evolution outdegree slight decreased of outdegress from 8 and 2, increse of 0
# 4b. Plot the affective network
# color nodes by gender and save coordinates
par(mfrow=c(1,2))
aff_plot_w1 <- gplot(affective_w1, main="affective network - Class 7400", vertex.col=sex)
aff_plot_w2 <- gplot(affective_w2, main="affective network - Class 7400", vertex.col=sex)
# 2. Study gender segregation

# first you need to load igraph (detach sna!)
library(igraph)


# put the network into an igraph graph object


graph_w1 <- graph.adjacency(affective_w1)
graph_w2 <- graph.adjacency(affective_w2)
class(sex)
sex<-as.numeric(sex)
# choose the appropriate assortativity function for gender and use it
# second, we asses whether there is gender homophily in affective choice
assortativity.nominal(graph_w1, sex) #0.2063204
assortativity.nominal(graph_w2, sex) # 0.307123 increase by 50%
myLayout <- layout.fruchterman.reingold(graph_w1)
plot(graph_w1,
     vertex.color = ifelse(sex == 1, "red", "darkblue"),
     vertex.shape = ifelse(sex == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.1,
     vertex.size = 4,
     vertex.label = "",
     layout = myLayout,
     main = "affective network")
gg.1 <-  affective_w1[sex==2, sex==2]
gb.1 <-  affective_w1[sex==2, sex==1]
bb.1 <-  affective_w1[sex==1, sex==1]
bg.1 <-  affective_w1[sex==1, sex==2]

gg.2 <-  affective_w2[sex==2, sex==2]
gb.2 <-  affective_w2[sex==2, sex==1]
bb.2 <-  affective_w2[sex==1, sex==1]
bg.2 <-  affective_w2[sex==1, sex==2]
aff.selection <- matrix(NA, 2, 2)
rownames(aff.selection) <- c("girl", "boy")
colnames(aff.selection) <- c("girl", "boy")
aff.selection.2 <- matrix(NA, 2, 2)
rownames(aff.selection.2) <- c("girl", "boy")
colnames(aff.selection.2) <- c("girl", "boy")
# it looks like this
aff.selection # rownames: sex of sender; colnames: sex of receiver

# the igraph density function is quite inflexible, let's switch back to sna
detach(package:igraph)
library(sna)

len=30

# fill in the selection table with the subgraph densities
# 3. affective selection table by gender


aff.selection[1,1] <- gden(gg.1, diag=FALSE)
aff.selection[1,2] <- gden(gb.1, diag=TRUE)
aff.selection[2,2] <- gden(bb.1, diag=FALSE)
aff.selection[2,1] <- gden(bg.1, diag=TRUE)
aff.selection

aff.selection.2[1,1] <- gden(gg.2, diag=FALSE)
aff.selection.2[1,2] <- gden(gb.2, diag=TRUE)
aff.selection.2[2,2] <- gden(bb.2, diag=FALSE)
aff.selection.2[2,1] <- gden(bg.2, diag=TRUE)
aff.selection.2

# normalize the results by the overall density
aff.selection.norm <- aff.selection / gden(affective_w1)
aff.selection.norm.2 <- aff.selection.2 / gden(affective_w2)
#Simmlarity
# The Hamming distance
# perhaps the most simple dissimilarity or distance measure known to mankind
# there is an sna function for calculating the Hamming distance
?hdist # counts the number of ties that are in different states (1-0)
(hamming <- hdist(affective_w1, affective_w2))
# but we should probably divide this raw number by the the maximum possible distance,
# which is when all ties are in different states
(hamming.prop <- hamming/nties(affective_w1)) # distance proportionate to max distance
#0.1022989 percent different 
# The simple matching coefficient
# a similar measure but from the point of view of similarity/stability instead of
# distance/change
(matching <- 1 - hamming.prop)
# it seems like it is a super-stable affective network
# however, as we saw earlier in the case of grecip, for sparse networks one will always get
# high similarity (or low distance), because most of the ties are absent

# The Jaccard index
# a more useful measure for us: it disregards ties that are absent in both networks
# and only considers these cases
#                network1    network2
#             A:    1           1
#             B:    1           0 
#             C:    0           1
#            ------------------------
# Jaccard index = A / (A + B + C)
A <- sum((affective_w1 * affective_w2)==1, na.rm=TRUE) # #ties that exist in both networks
BplusC <- sum((affective_w1 + affective_w2)==1, na.rm=TRUE) # #ties that exist in only one network
(jaccard <- A/(A+BplusC))
# this means that only a less than 40 % of the ties that
# existed in at least one observation were stable
# so actually, it turns out that the affective network was dynamically evolving


### 3. CORRELATIONS BETWEEN NETWORKS - THE QUADRATIC ASSIGNMENT PROCEDURE (QAP)

# functions that do QAPs for you in the sna package
library(sna)
?netlm
?netlogit

qap1 <- netlogit(affective_w2, affective_w1, nullhyp="qap", reps=1000)
sum.qap<-summary(qap1)
results<-NULL
results<-as.data.frame(results)
beta<-sum.qap[[1]]
beta<-as.data.frame(beta)
beta$exp.b<-exp(beta$beta)
beta$Prleeq <-sum.qap[["pleeq"]]
beta$Prgrep <-sum.qap[["pgreq"]]             
beta$Prgreqabs<-sum.qap[["pgreqabs"]]
beta<-rbind(beta,c("","",sum.qap[["aic"]],"",""))               
beta<-rbind(beta,c("","",sum.qap[["bic"]],"",""))               
beta<-rbind(beta,c("","",sum.qap[["null.deviance"]],"",""))
beta<-rbind(beta,c("","",sum.qap[["deviance"]],"",""))
rownames(beta)<-c("intercept","affective_w1","aic","bic","null.dev","deviance")
xtable(beta)

# you need to create a same sex matrix
# help is around line 140 in the last script - the variable coding is the same, so it is easy
same.sex <- sex.orig%*%t(sex.orig)
same.sex[same.sex==2] <- 0
same.sex[same.sex==4] <- 1

drink.orig<-drink
head(drink)
nrow(drink)
drink.matrix
#1.b. What are the relations between gender, drinking, and affective in wave 1? How do these change by wave 2?
# seperately with ergm ,both together with saom
# gender homophily, gender (clustering) drinking homophiliy, drinking closure (clustering) 

# what is the effect of same sex on affective?

# IMPORTANT: variables always have to be in matrix form!
# so first we need to create a same sex matrix for this,
# which contains 1 if two students have the same sex and 0 otherwise
same.sex <- sex %*% t(sex) # %*% is matrix multiplication (not cell-wise)
# we have a matrix that contains the values 1 (both boys), 4 (both girls), 2 (opposite sex)
# now we only have to recode it
same.sex[same.sex==2] <- 0
same.sex[same.sex==4] <- 1
drink_w1<-c(drink[,1])
drink_w2<-c(drink[,2])
names(drink_w1)
drink_w1_matrix<-matrix(data=NA,nrow=30,ncol=30)
rownames(drink_w1_matrix)<-names(drink_w1)
colnames(drink_w1_matrix)<-names(drink_w1)
#sender - reciever 
for (i in 1:30) { 
  for (j in 1:30) {
    drink_w1_matrix[i,j]<-as.numeric(drink_w1[i])-as.numeric(drink_w1[j])
  }
}
drink_w2_matrix<-matrix(data=NA,nrow=30,ncol=30)
for (i in 1:30) { 
  for (j in 1:30) {
    drink_w2_matrix[i,j]<-as.numeric(drink_w2[i])-as.numeric(drink_w2[j])
  }
}
#same drinking behaviour 1 different drinking behaviour 0
drink_same_matrix<-ifelse(drink_w1_matrix!=0,0,1)
#another hypothesis more drinking of sender has negative effect on friendship ties
drink_more_matrix<-ifelse(drink_w1_matrix>0,1,0)
drink_less_matrix<-ifelse(drink_w1_matrix<0,1,0)


# run QAP (note how we need to group the x variables in a list)
(qap3 <- netlogit(affective_w2, list(affective_w1, same.sex, drink_w1_matrix), nullhyp="qap", reps=100))
#same drinking seem to have no effect on predicting ties in affective network 2 ties controling for sex
#small positive effect <- weird 
(qap4 <- netlogit(affective_w2, list(affective_w1, same.sex,drink_more_matrix), nullhyp="qap", reps=100))
#not significant
(qap5 <- netlogit(affective_w2, list(affective_w1, same.sex,drink_less_matrix),nullhyp="qap", reps=100))
### Concluding remarks
# QAP is a nice and easy technique, and as you can see it can be applied to both
# cross-sectional and longitudinal data.
#
# However, in the end QAP is based on correlations between cell values, and trying to
# include anything more complicated than the dyad (e.g. triad closure) requires quite
# a bit of tricky data preparation. 


#2. Which “local rules” are important in explaining/predicting the structure of the two affective networks (separately)?
# ergm network structures

### INSTRUCTIONS
# 1.  Load your dataset from the RData file saved earlier.
# 2.  Run a dyad census on the friendship network of wave 2 and save the results in an object.
# 3.  Run a triad census on the friendship network of wave 2 and save the results in an object.
# 4.  Generate 100 random networks with the same size (number of actors) and density as your
#     friendship network of wave 2.
# 5.  Plot the distribution of dyads and triads in your friendship network against those of
#     the simulated networks.
# X.  In case you are done with everything, load the wave 2 trust network of your classroom,
#     repeat the exercise and try to identify the main structural similarities and differences
#     between friendship and trust networks.

### 2. DYAD, TRIAD, AND CYCLE CENSUS
dyad.count <- dyad.census(affective_w1)
dyad.count.2 <- dyad.census(affective_w2)
# count the different triad types
?triad.census
triad.count <- triad.census(affective_w1)

triad.count.2 <- triad.census(affective_w2)
triad.count.2
# what do these labels mean? - a short presentation slide

# there is also a function that identifies the type of a dyad
?triad.classify
# for example, what is the triadic relation between the first, fifth, and eigths actor?
triad.classify(affective_w1, tri=c(5,7,0))
triad.classify(affective_w2, tri=c(4,7,2))
#empty triad
#transitive triplet reciprotitive
# count cycles of different length
?kcycle.census
# it is a more delicate topic than counting dyads or triads - many possibilities
# let's count the cycles up to length 5 (don't go to high or it will take forever)
kcycle.census(affective_w1, maxlen=5)
kcycle.census(affective_w2, maxlen=5)
# oops, apparently the function does not like our absent students,
# so we have to remove them
# we do it in a new object in order not to ruin our original data
colnames(affective_w1)<-gsub("X","",colnames(affective_w1))
colnames(affective_w2)<-gsub("X","",colnames(affective_w2))
library(Hmisc)
temp <- affective_w1[row.names(affective_w1) %nin% c("7408","7412","7419","7425"), colnames(affective_w1) %nin% c("7408","7412","7419","7425")]
missing.names.2<-names(row.2[row.2==29])
temp.2 <- affective_w2[row.names(affective_w2) %nin% c(missing.names.2), colnames(affective_w2) %nin% missing.names.2]
# excluding rows and columns with the "-" sign
# and now it should work
cycles.w1<-kcycle.census(temp, maxlen=5, tabulate.by.vertex=FALSE)
#na replace with 0 else kcycle fails
temp.2<-ifelse(is.na(temp.2),0,temp.2)
cycles.w2<-kcycle.census(temp.2, maxlen=5, tabulate.by.vertex=FALSE)
# 4. Generate 100 random networks wit the same size and density as the friendship network

# you need to get the size and the density
aff1_size <- nrow(affective_w1)
aff1_dens <- gden(affective_w1)
aff2_size <- nrow(affective_w2)
aff2_dens <- gden(affective_w2)
# and use them as parameters when you generate the networks
### Bernoulli Random Graphs
# What are they?
# Tie values are drawn from a Bernoulli/binary distribution with success probability p.
# For example, if p=0.5 then there is a fifty-fifty chance that a tie is present in the
# network.
# Key features of Bernoulli Random Graphs:
# Ties are independent: whether a tie is present or not does not alter the probability
# of another tie being present.
# The expected value of the density of the graph is p.
###


# 4. TESTING THE TRIAD CENSUS AGAINST RANDOM NETWORKS

# here are the results from the triad census in the friendship network again
triad.count
triad.count.2
# how do should we interpret these numbers? are they high? are they low?
# answer: it depends
# for example on the number of actors - many actors means many possible triads
# or on the density of the network - there are many empty triads in a sparse network

# a natural way to interpret the triad census is by comparing the observed number of
# the different triads to the numbers we would see in a SIMILAR NETWORK in which ties
# are RANDOMLY PRESENT or absent

# we have already discussed what RANDOMLY PRESENT means
# but what is a SIMILAR NETWORK?
# networks can be similar or different in many ways, so it is up to you to decide
# what are the important aspects
# the most simple assumption is that two networks are similar if they have the
# same number of actors and the same density
# let's simulate 200 similar random networks
random.nets <- rgraph(aff1_size, 200, aff1_dens)
random.nets.2 <- rgraph(aff2_size, 200, aff2_dens)
par(mfrow=c(2,2))
gplot(affective_w1)
gplot(random.nets) # looks quite homogeneous, maybe too much

gden(random.graph) # density probably close to the expected value (0.1)
par(mfrow=c(2,2))
hist(degree(random.nets, cmode="indegree"), main="Indegee random graph")
hist(degree(affective_w1, cmode="indegree"), main="Indegee affective graph 1")
hist(degree(random.nets, cmode="outdegree"),main="outdegee random graph") # these are a bit different from what we usually see
hist(degree(affective_w1, cmode="outdegree"), main="outdegee affective graph 1")

# how does the result look like
dim(random.nets)
# it's a 200*160*160 array - this means that the simulated networks are
# identified by the first dimension
gplot(random.nets[1,,]) # this is the first random network
gplot(random.nets[2,,]) # this is the second

# are the densities really distribute around the density of the friendship network?
random.dens <- gden(random.nets)
random.dens.2 <- gden(random.nets.2)
hist(random.dens)
hist(random.dens.2)
mean(random.dens)
mean(random.dens.2)
aff1_dens
aff2_dens
# it seems so. good. it would be even better with more random networks

# to compare the observed triad counts to the simulated ones,
# we need to run the triad census on all 200 random networks
random.triad <- triad.census(random.nets)
random.triad.2 <- triad.census(random.nets.2)
# it takes a bit of time...
# what is the result?
class(random.triad) # a matrix
dim(random.triad) # containing count for the 16 triads (columns) in each network (rows)

library(vioplot)

# the plotting function we use has the same name as the package
?vioplot

# to get familiar with this kind of plot, let's look at
# the distribution of transitive triplets (030T) in the simulated networks
vioplot(random.triad[,9],                 # the distribution
        names=colnames(random.triad)[9],  # name of the triad type
        col="transparent",                # let the "violin" be transparent
        ylim=c(0, 100))                    # displayed part of the y axis
# compared to this, where is the number that we actually observed in the friendship networs?
# we add the observed value as a point to the plot
points(1, triad.count[9],                 # x and y coordinates for the added point
       col="red",                         # color of point
       pch=15)                            # shape of point: solid rectangle

# now we plot the distribution of all closed triangles (in a quite primitive way)
vioplot(random.triad[,9], random.triad[,10], random.triad[,12],
        random.triad[,13], random.triad[,14], random.triad[,15], 0,
        names=colnames(random.triad)[c(9,10,12,13,14,15,16)],
        col="transparent")
# and mark the observed numbers in each category
points(1:7,
       triad.count[c(9,10,12,13,14,15,16)],
       col="red",
       type="b",
       pch=15)
# quite different to random graph, hence structures seem to be non random

# 5. Plot the dyad and triad distributions in a violinplot

# first you need to repeat steps 2 and 3 for the simulated networks.
simaffect_dyad<-dyad.census(random.nets)
simaffect2_dyad<-dyad.census(random.nets.2)
simaffect_triad<-triad.census(random.nets)
simaffect2_triad<-triad.census(random.nets.2)
vioplot(simaffect_dyad[,1], simaffect_dyad[,2],simaffect_dyad[,3], names=colnames(simaffect_dyad) )
points(1:3,
       dyad.count[c(1,2,3)],
       col="red",
       type="b",
       pch=15)
#more mutual and (alot) less asym and null dyads than in random
vioplot(simaffect2_dyad[,1], simaffect2_dyad[,2],simaffect2_dyad[,3], names=colnames(simaffect2_dyad) )
points(1:3,
       dyad.count.2[c(1,2,3)],
       col="red",
       type="b",
       pch=15)
#more mutual and (alot) less asym and null dyads than in random

### OVERVIEW
# You will work on your RECENS classroom again. Your task is to build ERGMs
# that model clustering and homophily by gender. Then you need to assess the GOF, too.
###
library(statnet)

# 2.  Build up an ERGM for friendship w1 step by step including the following effects
#                       edges
#                       mutuality
#                       clustering
#                       sender, receiver, and homophily effects for gender
friendship_w1 <- affective_w1 # first put the original network into a friend object
friendship_w2 <- affective_w2
# to make things more simple, we should use objects of class "network"
friend1 <- network(friendship_w1)
friend2 <- network(friendship_w2)
# you can add vertex/node/actor attributes to a network object by
# using the %v% operator

friend2 %v% "drinking" <- as.numeric(drink_w2)

friend2 %v% "sex" <- as.numeric(sex.orig)

friend1 %v% "drinking" <- as.numeric(drink_w1)

friend1 %v% "sex" <- as.numeric(sex.orig)

# 4. Assess the convergence of the full models.
### 6. STRUCTURAL AND COVARIATE EFFECTS

# reciprocity is a universal tendency in friendships - include it in the model
ergm2 <- ergm(friend1~edges+mutual)
ergm3 <- ergm(friend2~edges+mutual)
# the estimation looks more serious now
# some parts of it are done in C which communicates with R - :-O (shocked face)
# interested in how the algorithm works? check ?ergm

# a useful option, good for debugging bad models:
# use the verbose=TRUE argument to get more feedback about the estimation
ergm2 <- ergm(friend1~edges+mutual, verbose=TRUE)

# now look at the results
ergm.edge.mutal.w1<-summary(ergm2)
ergm.edge.mutal.w2<-summary(ergm3)
# MINITASK: How can you interpret the reciprocity parameter?

# how about clustering?
ergm3 <- ergm(friend1~edges+mutual+gwesp(alpha=0, fixed=TRUE))
ergm4 <- ergm(friend2~edges+mutual+gwesp(alpha=0, fixed=TRUE))
summary(ergm3)
summary(ergm4)
#marginal effectsby hand
# this took more time... did the estimation converge?
mcmc.diagnostics(ergm4)
# quite well; in case you see the trace plots going up, down or oscillating,
# you can continue the estimation starting from where you left off
ergm3b <- ergm(friend1~edges+mutual+gwesp(0, fixed=T), control=control.ergm(init=ergm3$coef))
ergm4b <- ergm(friend1~edges+mutual+gwesp(0, fixed=T), control=control.ergm(init=ergm4$coef))
mcmc.diagnostics(ergm3b)
mcmc.diagnostics(ergm4b)
# this is probably better
# watch out: if your model is stuck in a local trap (degeneracy), continuing the estimation
# might make things worse and worse by every run...

# look at the results
summary(ergm3b)
summary(ergm4b)
# but what does 'gwesp.fixed.0' mean? clustering
#source: Using Exponential Random Graph Models to Investigate Adolescent Social Networks 
#The GWESP statistic defines a parametric form of this count distribution that gives
#each additional shared partner a declining positive impact on the probability of two persons forming a tie
#triad closure
# what are the potential confounding factors causing clustering?
ergm5 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star)
mcmc.diagnostics(ergm5)
ergm5b<-ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star, control=control.ergm(init=ergm5$coef))
mcmc.diagnostics(ergm5b)
summary(ergm5b)
ergm6 <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star)
ergm6b <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star,control=control.ergm(init=ergm6$coef))
#Snijders et al. (2006) and Robins et al. (2007) provided interpretations of the associated parameter: 
#a positive estimate is evidence that the network contains a skewed degree distribution with some higher degree nodes,
#whereas a negative parameter suggests that
mcmc.diagnostics(ergm6b)
summary(ergm6b)

#m2star counting the cases a node has incoming and outgoing ties.
#istar
#fit both with ist ostar model and without
ergm7 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex"))


ergm10 <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex"))


ergm11<-ergm(friend2~edges+mutual+gwesp(0, fixed=T)  +m2star+
       nodematch("sex") )
ergm11b<-ergm(friend2~edges+mutual+gwesp(0, fixed=T)  +m2star+
               nodematch("sex") , control=control.ergm(init=ergm11$coef))

mcmc.diagnostics(ergm11b)
#doenst converge
ergm8 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex")+nodematch("drinking"))

ergm9 <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex")+nodematch("drinking"))
ergm9b <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex")+nodematch("drinking"), contol=control.ergm(init=ergm9$coef))



# print the gof plots to pdf's
#xaxis possible indegree
#yaxis proportion with as many 
#if solid line within the dashed line of the simulted values
#focus on all different measures
# pdf("ergm1_gof.pdf")
# plot(ergm1_gof)
# dev.off()
# 
# pdf("ergm4_gof.pdf")
# plot(ergm4_gof)
# dev.off()
# 
# pdf("ergm6_gof.pdf")
# plot(ergm6_gof)
# dev.off()

# let's inspect the results


### 8. SIMULATING NETWORKS FROM AN ERGM MODEL

# the last topic in this basic intro to the statnet package
# is about simulating random networks from a model that you
# estimated using the ergm function


# mean imputation therefore SE will be downward biased, but we cant think of an good imputation model to generate multiple imputations
drink_mean_imp_w1<-drink_w1
for (i in 1:length(drink_w1)) { 
  if( is.na(drink_w1[i])) { 
  drink_mean_imp_w1[i]<-round(mean(drink_w1, na.rm=T),0)
  } 
  else { 
    drink_mean_imp_w1[i]<-drink_w1[i]
    }
}
drink_mean_imp_w2<-drink_w2
for (i in 1:length(drink_w2)) { 
  if( is.na(drink_w2[i])) { 
    drink_mean_imp_w2[i]<-round(mean(drink_w2, na.rm=T),0)
  } 
  else { 
    drink_mean_imp_w2[i]<-drink_w2[i]
  }
}
drink.alot<-ifelse(drink_mean_imp_w2>3,1,0)
friend1 %v% "mean_imp_drink"<- as.numeric(drink_mean_imp_w1)
friend2 %v% "mean_imp_drink"<- as.numeric(drink_mean_imp_w2)
friend2 %v% "drink.alot"<-as.numeric(drink.alot)
ergm12 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                 nodematch("sex")+nodematch("mean_imp_drink"))


ergm13 <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                 nodematch("sex")+nodecov("drink.alot"))
ergm14 <- ergm(friend2~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                 nodematch("sex")+nodematch("drink.alot"))



#clustering no star
gof_ergm3b<-gof(ergm3b)
gof_ergm4b<-gof(ergm4b)
#clustering star
gof_ergm5b<-gof(ergm5b)
gof_ergm6<-gof(ergm6)
#last model with sex
gof_ergm7<-gof(ergm7)
gof_ergm10<-gof(ergm10)
#with sex drinking
gof_ergm8<-gof(ergm8)
gof_ergm9b<-gof(ergm9b)
#with mean imputed drinking
gof_ergm14<-gof(ergm14)

# it is also very easy (technically speaking)
?simulate # note that this function comes with the stat package - not ergm-specific!
mynets <- simulate(ergm10, 100) # simulates 10 networks from model 10
mynets
# first you need to repeat steps 2 and 3 for the simulated networks.
simnets_dyad_ergm<-dyad.census(mynets)

simnets_triad_ergm<-triad.census(mynets)

# to get familiar with this kind of plot, let's look at
# the distribution of transitive triplets (030T) in the simulated networks
vioplot(simnets_triad_ergm[,9],                 # the distribution
        names=colnames(simnets_triad_ergm)[9],  # name of the triad type
        col="transparent",                # let the "violin" be transparent
        ylim=c(0, 100))                    # displayed part of the y axis
# compared to this, where is the number that we actually observed in the friendship networs?
# we add the observed value as a point to the plot
points(1, triad.count.2[9],                 # x and y coordinates for the added point
       col="red",                         # color of point
       pch=15)                            # shape of point: solid rectangle

# now we plot the distribution of all closed triangles (in a quite primitive way)
vioplot(simnets_triad_ergm[,9], simnets_triad_ergm[,10], simnets_triad_ergm[,12],
        simnets_triad_ergm[,13], simnets_triad_ergm[,14], simnets_triad_ergm[,15],simnets_triad_ergm[,16],
        names=colnames(simnets_triad_ergm)[c(9,10,12,13,14,15,16)],
        col="transparent")
# and mark the observed numbers in each category
points(1:7,
       triad.count.2[c(9,10,12,13,14,15,16)],
       col="red",
       type="b",
       pch=15)
# quite different to random graph, hence structures seem to be non random

# 5. Plot the dyad and triad distributions in a violinplot


vioplot(simaffect_dyad[,1], simaffect_dyad[,2],simaffect_dyad[,3], names=colnames(simaffect_dyad) )
points(1:3,
       dyad.count.2[c(1,2,3)],
       col="red",
       type="b",
       pch=15)
#more mutual and (alot) less asym and null dyads than in random
vioplot(simaffect2_dyad[,1], simaffect2_dyad[,2],simaffect2_dyad[,3], names=colnames(simaffect2_dyad) )
points(1:3,
       dyad.count.2[c(1,2,3)],
       col="red",
       type="b",
       pch=15)
#drinking not significant neither as covariate nor as match
mcmc.diagnostics(ergm13)
#nodematch persons with same sex more likely to connect
mcmc.diagnostics(ergm7)
mcmc.diagnostics(ergm8)
mcmc.diagnostics(ergm9b)

summary(ergm7)
summary(ergm8)
summary(ergm9)
summary(ergm10)

#3. Which micro mechanisms shape the evolution of the affective network in your classroom?
#sienna
nActors <- dim(affective_w1)[1]


######### Step 2: Create the RSiena objects #########

# now that all data is loaded into R, we can 
# construct the type of data that RSiena understands
detach(package:statnet)
detach(package:ergm.count)
detach(package:tergm)
detach(package:ergm)

detach(package:networkDynamic)
detach(package:network)
detach(package:sna)
# create the longitudinal network object
library(RSiena)
library(igraph)
?sienaDependent
nActors <- dim(affective_w1)[1]
myNetwork <- sienaDependent( array( c(affective_w1, affective_w2 ), 
                                    dim = c(nActors, nActors, 2 ) ) )
myNetwork


# create constant actor covariates
gender.coCovar <- coCovar(as.numeric(sex))

# create the varying dyadic covariate
drink.mean.imp<-ifelse(is.na(drink),round(mean(drink,na.rm=T),0),drink)
#drinking can be only included as constant variable as we have only two waves
drink.mean.imp<-rowSums(drink.mean.imp)/2

drink.covar<-coCovar(drink.mean.imp)

# now create an object that includes all the information necessary for a Siena analysis

?sienaDataCreate
myData <- sienaDataCreate(myNetwork, gender.coCovar, drink.covar) 
myData

# Including the third observation of the dyadic covariate (distance) would have
# returned an error when calling sienaDataCreate:
#                 Error in dyvCovars[[i]][, 1:(observations - 1)] : 
#                   incorrect number of dimensions
# It is because sienaDataCreate expects 2 observations for the varying covariate:
# covariates are not modeled, and their state in t3 is irrelevant for estimation.

# print report to check
print01Report(myData, modelname="Mannheim_7400")


######### Step 3: Specify a model #########

# create an effects object for the created data

?getEffects
myEff <- getEffects(myData)

#look at your effects object

myEff
names(myEff)

# see all the short names of available effects
myEff$effectName
myEff$shortName

# a more civilized list
effectsDocumentation(myEff)

# include the structural effects
?includeEffects
myEff <- includeEffects(myEff, transTrip)
myEff <- includeEffects(myEff, cycle3, transRecTrip,  inActSqrt, outActSqrt)
# include covariate related effects

myEff <- includeEffects(myEff, inPop)

# homophily effects and ego alter control
myEff <- includeEffects(myEff, egoX, altX, sameX, interaction1="gender.coCovar")
myEff <- includeEffects(myEff, egoX, altX, sameX, interaction1="drink.covar")

myModel <- sienaAlgorithmCreate(useStdInits=FALSE, projname="My Model")
#initial parameter values from previous research included
# Print a report to a file that includes all important information on the data and model you specified

print01Report(myData, modelname="My Model")

# Now, finally, we can actually run the model


######### Step 5: Estimate #########

?siena07
myAnswer.mannheim <- siena07(myModel, data = myData, effects = myEff, returnDeps = T)
#returnDeps returns simulation results
myAnswer.mannheim

# if convergence is not satisfying or we have a model that takes long to
# get to the proper values, we can run again with new starting values

myAnswer.mannheim <- siena07(myModel, data = myData, effects = myEff, returnDeps = T,
                             prevAns=myAnswer.mannheim, # start with previous results
                             useCluster=TRUE, nbrNodes=4) 


siena.table(myAnswer.mannheim,
            type="html",
            file="Mannheim_result.html",
            tstatPrint=TRUE,
            sig=TRUE,
            d=2)

# All results are also saved in the file "My Model.out" in the working directory
# check the t-ratios for convergence!!!
# only then proceed to looking at the results



######## Ego-alter selection tables #######

# We have estimated the model and got the parameters expressing gender homophily
# So, is it more or less likely that a girl nominates another girl as a friend
# than for a boy to nominate a boy as a friend?

# An ego-alter selection table is nice represenation of the results
# (see section 13.3 of the RSiena Manual for further information)

# LET'S LOOK AT SOME SLIDES
# ...

# having done that, look at the first few values of the gender variable in the
# Siena data object

myData$cCovars$sex[1:10]

# here is the mean on which the centering is based

attr(myData$cCovars$sex, "mean")

# Note: you can tell RSiena not to center covariates:
# when you define a covariate, use the "centered=FALSE" argument
# (in the functions coCovar, varCovar, coDyadCovar, varDyadCovar)
# but it is advised to center them.

# Now, with the parameter estimates and the mean of the gender variable,
# we can easily do the selection table for sex covariate effects.

#4. State one hypothesis about other micro mechanisms which you could test with the learnt methods.

