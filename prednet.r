require(h2o)
h2o.init()
h2o.no_progress() 
require(igraph)
require(caret)

########## Analise Descritiva ############

pdf("demograficos.pdf", height=10, width=10)
dados = as.matrix(read.table("dados_AMOSTRA1.CSV", header=TRUE, sep=";"))
dados = dados[, 7:168]
dados2 = matrix(0, nrow(dados), ncol(dados))
dados2[which(dados == "-1")] = NA
dados2[which(dados == "99")] = NA
for(i in 1:ncol(dados)){dados2[, i] = as.numeric(dados[, i])}
colnames(dados2) = colnames(dados)
rm(dados)

par(mfrow =c(2, 2))
#VB01001 - sexo
z = table(dados2[, 2])
z = z/sum(z)
names(z) = c(paste("Male (", round(z[1]*100, 1),"%)", sep=""),
             paste("Female (", round(z[2]*100,1),"%)", sep=""))
pie(z, col = c(4, 2), main = "Gender")

#VB01003 - idade
age = dados2[, 4]
hist(as.numeric(age), col = 8, xlab ="Age (years)", main = "Age", prob = TRUE)

#VB01002 - raca
z = table(dados2[, 3])
z = z/sum(z)
names(z) = c(paste("White (", round(z[1]*100, 1),"%)", sep=""),
             paste("Black (", round(z[2]*100, 1),"%)", sep=""),
             paste("Asian (", round(z[3]*100, 1),"%)", sep=""),
             paste("Pardo (", round(z[4]*100, 1),"%)",sep=""),
             paste("Indigenous (", round(z[5]*100, 1),"%)", sep=""))
pie(z, col = c(0, 1, 7, 2, 3), main = "Ethnicity")


#VB01022 - turno
z = table(dados2[, 22])
z = z/sum(z)
names(z) = c(paste("Morning (", round(z[1]*100, 1),"%)", sep=""),
             paste("Intermediate (", round(z[2]*100, 1),"%)", sep=""),
             paste("Afternoon (", round(z[3]*100, 1),"%)", sep=""),
             paste("Night (", round(z[4]*100, 1),"%)", sep=""),
             paste("Full-time (", round(z[5]*100, 1),"%)", sep=""))
pie(z, col = c(3, 1, 4, 5, 2), main="Time at School")
dev.off()
rm(dados2)

######### PIPELINE ANALYSIS ############

#Matriz com a AUC de cada variavel em cada regiao do pais
AUC = matrix(0, 648, 5)

#Nome das regioes do pais
NomeRegiao = c("North", "North-East", "South-East", "South", "Center-West")

#Analise para cada REGIAO do PAIS
for(UF in 1:5){
    #for(UF in 3:4){
print(UF)

# Variaveis a serem consideradas
VARIAVEIS=c("VB01001","VB01002","VB01004","VB01022","VB01023","VB01024","VB01025","VB01026","VB01006","VB01007","VB01010A","VB01013","VB01014","VB01015A","VB01016","VB01017","VB01018","VB01019","VB01020A","VB01008A","VB01011","VB02019A","VB02017A","VB02018A","VB02021","VB02020A","VB02001","VB02002","VB02004A","VB02010","VB02011","VB02013","VB02022","VB02023","VB02024","VB02025","VB02027","VB03001A1","VB03002A1","VB03001A2","VB03002A2","VB03003A","VB03006A","VB03007","VB03011A","VB03008","VB03010A","VB04001","VB04008A","VB04005","VB04006A","VB05002","VB05010","VB06001","VB06006","VB07001","VB07002","VB07003","VB07004","VB07005","VB07006","VB07007","VB07009","VB07010","VB12001","VB12002","VB12003","VB08001","VB08008","VB08009","VB08010","VB10004","VB10005","VB10006","VB10001A","VB10002","VB10003","VB09001","VB09002","VB09006A1","VB09006A2","VB09007A","VB09008","VB09009","VB09003","VB09004","VB09005","VB09010","VB09011","VB09012","VB09013A","VB09015","VB09016","VB13005","VB13006","VB13001","VB13004A","VB13008","VB14001","VB14002","VB11006","VB11007","VB11001","VB11002","VB11003","VB11004A","VB11005")
ZZ = VARIAVEIS


#ETL
dados = as.matrix(read.table("2015/dados_AMOSTRA1.CSV", header=TRUE, sep=";"))

#Seleciona UF
IX = which(dados[, 3] == UF)
dados = dados[IX,]

dados = dados[, 7:168]
dados2 = matrix(0, nrow(dados), ncol(dados))
dados2[which(dados == "-1")] = NA
dados2[which(dados=="99")] = NA
for(i in 1:ncol(dados)){dados2[, i] = as.numeric(dados[,i])}
colnames(dados2) = colnames(dados)
rm(dados)

#Filtra variaveis
dados3 = matrix(0, nrow(dados2), length(VARIAVEIS))
for(i in 1:length(VARIAVEIS)){
    IX = which(colnames(dados2) == VARIAVEIS[i])
    dados3[, i] = paste(dados2[, IX],"a", sep="")
}
rm(dados2)
colnames(dados3) = VARIAVEIS

#One hot enconding
dmy <- dummyVars(" ~.", data = dados3)
dados4 <- data.frame(predict(dmy, newdata = dados3))
rm(dados3)

#transforma todas as colunas em fatores
for(i in 1:ncol(dados4)){
    kk = which(dados4[, i] == 1)
    dados4[kk, i] = "Yes"
    dados4[-kk, i] = "No"
    dados4[, i] = factor(dados4[, i])
}

VARIAVEIS = colnames(dados4)

#prefixo do nome das variaveis
ROOTVARIAVEIS = VARIAVEIS
for(i in 1:length(VARIAVEIS)){
    ROOTVARIAVEIS[i] = strsplit(VARIAVEIS[i],"\\.")[[1]][1]
}

#Sobe para o h2o
dados_hf = as.h2o(dados4)
rm(dados4)

#Split data into Train/Validation/Test Sets
split_h2o <- h2o.splitFrame(dados_hf, c(0.7))
train_conv_h2o <- h2o.assign(split_h2o[[1]], "train" )
test_conv_h2o  <- h2o.assign(split_h2o[[2]], "test" )


GRAPH = matrix(0, length(VARIAVEIS), length(VARIAVEIS))
for(j in 1:length(VARIAVEIS)){
    #  print(j)
   #Model training
   aml <- h2o.gbm(y=VARIAVEIS[j], 
                  x = VARIAVEIS[-which(ROOTVARIAVEIS==ROOTVARIAVEIS[j])],
                  training_frame = train_conv_h2o,ntrees=100,max_depth=4)
                  #,distribution ="bernoulli")
   z = h2o.performance(aml, test_conv_h2o)
   AUC[j, UF] = h2o.auc(z)
   #Variance Importance
   z = h2o.varimp(aml)
   IMP = z[, 3]
   names(IMP) = z[, 1]
   for(i in 1:length(IMP)){
      GRAPH[j, which(VARIAVEIS == names(IMP)[i])] = IMP[i]
   }

   # Predict on hold-out test set
   #pred_conversion <- h2o.predict(object = aml, newdata = test_conv_h2o)
   #z2=h2o.performance(aml, test_conv_h2o)
   #GRAPH[j,i]=(h2o.mean_per_class_error(z2)-h2o.mean_per_class_error(z))/#h2o.mean_per_class_error(z2)
   
  }#for da variavel
  colnames(GRAPH) = VARIAVEIS
  write.table(GRAPH, paste("grafo", UF, ".txt", sep=""), col.names = T)
  print("AUC size:  ")
  print(dim(AUC))
}#for do UF
write.table(AUC,"AUC.txt",sep=";")

###########
#Graph Analysis

#sufixo das categorias dos clusters
CLUSTER = c('demog','demog','demog','escolar','escolar','escolar','escolar','escolar','casa','casa','casa','casa','casa','casa','casa','casa','casa','casa','casa', 'demog','demog','nutri','nutri','nutri','nutri','nutri','nutri','nutri','nutri', 'nutri','nutri','nutri','nutri','nutri','nutri','nutri','nutri','fisico','fisico','fisico','fisico','fisico','fisico','fisico','fisico','fisico','demog','drug','drug','drug','drug','drug','drug','drug','drug','auton','auton','auton','auton','auton','bully','bully','bully','bully','mental','mental','mental','sex','sex','sex','sex','higiene','higiene','higiene','higiene','higiene','saude','viol','viol','transporte','transporte','transporte','transporte','transporte','viol','viol','viol','viol','viol','viol','viol','transporte','viol','saude','saude','saude','saude','sex','saude','saude','saude','saude','saude','saude','saude','saude','saude')

ROOTCLUSTER = array(0, length(ROOTVARIAVEIS))
for(i in 1:length(ROOTVARIAVEIS)){
    ROOTCLUSTER[i] = CLUSTER[which(ZZ == ROOTVARIAVEIS[i])]
}

## AUC
NomeRegiao = c("North","North-East","South-East","South","Center-West")
AUC = read.table("AUC.txt",sep=";",header=TRUE)
colnames(AUC) = NomeRegiao
pdf("AUC.pdf")
boxplot(AUC, ylab = "AUC", xlab = "Region")
dev.off()

## GRAPH MODELLING
Z = NULL
par(mfrow = c(5, 1))

for(UF in 1:5){
 GRAPH = read.table(paste("grafo", UF, ".txt", sep=""), header=T)
 print(dim(GRAPH))
 g = graph.adjacency(t(GRAPH), weighted=TRUE, mode="directed", diag=FALSE)
 Z = cbind(Z, hub_score(g)$vector)
 rownames(Z) = colnames(GRAPH)
 barplot(Z[sort(Z[, UF], index.return=T, decreasing = T)$ix, UF][1:10])
}

############
VARIAVEIS = colnames(GRAPH)
#Media e Desvio-padrao across UFs
MD = NULL
for(i in 1:nrow(Z)){
    MD = rbind(MD, c(mean(Z[i, ]), sd(Z[i, ])))
}
rownames(MD) = VARIAVEIS
IX = sort(MD[, 1], index.return = T, decreasing = T)$ix
COLOR = array(0, length(VARIAVEIS))
COLOR[1:5] = 8
pdf("MeanHubScore.pdf")
barplot(MD[IX[1:50], 1], col = COLOR, xlab = "Variables", ylab = "Mean hub score")
dev.off()
barplot(t(MD[IX,][1:5,]), beside = T, col = c(2, 8))

#Correlacao across UFs
pdf("scatter.pdf", height = 10, width = 10)
MeanRho = NULL
par(mfrow = c(ncol(Z), ncol(Z)))
for(i in 1:ncol(Z)){
    for(j in 1:ncol(Z)){
       pearson = round(cor(Z[, i], Z[, j]), 3)
       if(i == j){
           hist(Z[, i], prob = T, xlab = NomeRegiao[i], main = NomeRegiao[i])
       }
       else{
          MeanRho = c(MeanRho, pearson)
          plot(Z[, i], Z[, j], xlab = NomeRegiao[i], ylab = NomeRegiao[j], pch = ".")
          legend("bottomright", paste("r=", pearson, sep=""), box.col = 0, cex = 1.25)
       }
}}
dev.off()

mean(MeanRho)




##########
#1- VB01001.1A - Sexo
#2- VB02018A.1A    Você costuma comer quando está assistindo à TV ou estudando? Sim, todos os dias
#3- VB02023.1A    NOS ÚLTIMOS 7 DIAS, em quantos dias você comeu em restaurantes fast food, tais como lanchonetes, barracas de cachorro quentes, pizzaria etc?  Não comi em restaurantes fast food nos últimos 7 dias (0 dia)
#4- VB10001A.6A - NOS ÚLTIMOS 30 DIAS, quantas vezes por dia você usualmente escovou os dentes?: 4 ou mais vezes por dia nos últimos 30 dias
#5- VB01025.5A - Escolaridade pretendida: Pos-graduacao


#Ideia PAPER
#Figura 1 - Diagrama
#Figura 2 - Demograficos
#Figura 3 - AUC
#Figura 4 - VAR IMP
#Figura 5 - Matriz de Scatter-plot e correl de Z

