###########################################################################
# PC Algorithm using the pcalg package on the CDP dataset
# This code accompanies Gururaghavadran and Murray, 2023
# Available at https://github.com/eleanormurray/CausalDiscovery_CDP
###########################################################################

# Code Section 0 - Data Setup ---------------------------------------

# Load the following packages. If they do not exist on your computer,
# this code will automatically install them into your default library. 
# To set working directory
if(!require(reshape2)) { install.packages("reshape2"); require(reshape2)}
# For tidy code
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse)}


library(bnlearn)
library(pcalg)
library(micd)
library(tpc)

# Load the data from the trial
trial <- read.csv("C:\\Users\\ejmurray\\Dropbox\\ProjectManagement\\DAGopedia\\Auto_DAGs\\Rajesh\\cdp_binary.csv", header=TRUE,  na="")
trial2 <- read.csv("C:\\Users\\ejmurray\\Dropbox\\ProjectManagement\\DAGopedia\\Auto_DAGs\\Rajesh\\cdp_expertDAG.csv", header=TRUE,  na="")

#convert risk group to values 0,1,2 (from 1,2,3)
trial$IRK<-as.integer(trial$IRK-1)
trial2$IRK<-as.integer(trial2$IRK - 1)

#Omit individuals with any missingness
trials_complete<-na.omit(trial)
trials2_complete<-na.omit(trial2)

#Variables in datasets
names(trials_complete)
names(trials2_complete)

#Set trial dataset for PC algorithm: suffStat  = CDP1980 variable list; suffStat2 = expanded expert variable list
suffStat<-list(dm=trials_complete, adaptDF=FALSE)
suffStat2<-list(dm=trials2_complete, adaptDF=FALSE)

#Custom plot output
mygraph <- function(pcgraph, a, b){
  g <- as.bn(pcgraph, check.cycles = FALSE)
  graphviz.plot(g, shape = "ellipse", main = paste("cpDAG, alpha =", a, "variable list =", b))
}


####################################
#Original variable list (suffStat)

##############################
#run PC algorithm, alpha = 0.01
pcalg_fit_mix <- pc(suffStat=suffStat, 
                    indepTest =disCItest, alpha = 0.01,
                    labels = colnames(trials_complete), 
                    skel.method = "stable", verbose = TRUE,
                    maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat<-(as(pcalg_fit_mix, "matrix") != 0) 
fit.mat
print.table(1*fit.mat, zero.=".")


#basic plot of edge matrix
plot(as(t(fit.mat), "graphNEL"), main = "alpha = 0.01, original variables")

mygraph(pcalg_fit_mix, 0.01, "original")

###############################
#run PC algorithm, alpha = 0.10
pcalg_fit_mix2 <- pc(suffStat=suffStat, 
                    indepTest =disCItest, alpha = 0.10,
                    labels = colnames(trials_complete), 
                    skel.method = "stable", verbose = TRUE,
                    maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat2<-(as(pcalg_fit_mix2, "matrix") != 0) 
fit.mat2
print.table(1*fit.mat2, zero.=".")

plot(as(t(fit.mat2), "graphNEL"), main = "alpha = 0.10, original variables")
mygraph(pcalg_fit_mix2, 0.10, "original")

###################################
#run PC algorithm, alpha = 0.05
pcalg_fit_mix3 <- pc(suffStat=suffStat, 
                     indepTest =disCItest, alpha = 0.05,
                     labels = colnames(trials_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat3<-(as(pcalg_fit_mix3, "matrix") != 0) 
fit.mat3
print.table(1*fit.mat3, zero.=".")

plot(as(t(fit.mat3), "graphNEL"), main = "alpha = 0.05, original variables")
mygraph(pcalg_fit_mix3, 0.05, "original")


###############################
#run PC algorithm, alpha = 0.20
pcalg_fit_mix4 <- pc(suffStat=suffStat, 
                     indepTest =disCItest, alpha = 0.2,
                     labels = colnames(trials_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat4<-(as(pcalg_fit_mix4, "matrix") != 0) 
fit.mat4
print.table(1*fit.mat4, zero.=".")

plot(as(t(fit.mat4), "graphNEL"), main = "alpha = 0.20, original variables")
mygraph(pcalg_fit_mix4, 0.20, "original")


###############################
#run PC algorithm, alpha = 0.001
pcalg_fit_mix5 <- pc(suffStat=suffStat, 
                     indepTest =disCItest, alpha = 0.001,
                     labels = colnames(trials_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat5<-(as(pcalg_fit_mix5, "matrix") != 0) 
fit.mat5
print.table(1*fit.mat5, zero.=".")


plot(as(t(fit.mat5), "graphNEL"), main = "alpha = 0.001, original variables")
mygraph(pcalg_fit_mix5, 0.001, "original")



####################################
#Updated variable list (suffStat2)

##############################
#run PC algorithm, alpha = 0.01
pcalg_fit_mixb <- pc(suffStat=suffStat2, 
                    indepTest =disCItest, alpha = 0.01,
                    labels = colnames(trials2_complete), 
                    skel.method = "stable", verbose = TRUE,
                    maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.matb<-(as(pcalg_fit_mixb, "matrix") != 0) 
fit.matb
print.table(1*fit.matb, zero.=".")

#basic plot of edge matrix
plot(as(t(fit.matb), "graphNEL"), main = "alpha = 0.01, updated variables")

mygraph(pcalg_fit_mixb, 0.01, "updated")

###############################
#run PC algorithm, alpha = 0.10
pcalg_fit_mix2b <- pc(suffStat=suffStat2, 
                     indepTest =disCItest, alpha = 0.10,
                     labels = colnames(trials2_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat2b<-(as(pcalg_fit_mix2b, "matrix") != 0) 
fit.mat2b
print.table(1*fit.mat2b, zero.=".")

plot(as(t(fit.mat2b), "graphNEL"), main = "alpha = 0.10,  updated variables")
mygraph(pcalg_fit_mix2b, 0.10, "updated")

###################################
#run PC algorithm, alpha = 0.05
pcalg_fit_mix3b <- pc(suffStat=suffStat2, 
                     indepTest =disCItest, alpha = 0.05,
                     labels = colnames(trials2_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat3b<-(as(pcalg_fit_mix3b, "matrix") != 0) 
fit.mat3b
print.table(1*fit.mat3b, zero.=".")

plot(as(t(fit.mat3b), "graphNEL"), main = "alpha = 0.05, updated variables")
mygraph(pcalg_fit_mix3b, 0.05, "updated")


###############################
#run PC algorithm, alpha = 0.20
pcalg_fit_mix4b <- pc(suffStat=suffStat2, 
                     indepTest =disCItest, alpha = 0.2,
                     labels = colnames(trials2_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat4b<-(as(pcalg_fit_mix4b, "matrix") != 0) 
fit.mat4b
print.table(1*fit.mat4b, zero.=".")

plot(as(t(fit.mat4b), "graphNEL"), main = "alpha = 0.20, updated variables")
mygraph(pcalg_fit_mix4b, 0.20,  "updated")


###############################
#run PC algorithm, alpha = 0.001
pcalg_fit_mix5b <- pc(suffStat=suffStat2, 
                     indepTest =disCItest, alpha = 0.001,
                     labels = colnames(trials2_complete), 
                     skel.method = "stable", verbose = TRUE,
                     maj.rule = TRUE, solve.confl = TRUE)

#Output edge matrix & print with zeros displayed as .
fit.mat5b<-(as(pcalg_fit_mix5b, "matrix") != 0) 
fit.mat5b
print.table(1*fit.mat5b, zero.=".")


plot(as(t(fit.mat5b), "graphNEL"), main = "alpha = 0.001, updated variables")
mygraph(pcalg_fit_mix5b, 0.001,  "updated")
