####Aula1####

install.packages("dplyr", dependencies = T)
install.packages("tidyr", dependencies = T)

#carregar um pacote
library(dplyr)

#Ajuda
??sequence
help(seq)
ex1 <- seq(0, 10, 0.2)
length(ex1)

#nome de variaveis

altura <- c(1.82, 1.92, 1.61, 1.68, 1.62)
altura1 <- 1.92
altura_jesse <- 1.92 
altura.poli <- 1.61
altura.poli = 1.61

1.61 -> poli

#verificar o tipo de uma variavel
is.numeric(poli)
is.character(poli)

poli1 <- as.character(poli)
is.numeric(poli1)

#remover
rm(Altura)

#vetor
altura <- c(1.82, 1.92, 1.61, 1.68, 1.62)
nomes <- c("wesley", "jesse", "poli",
           "isa", "pedro")
nome.alt <- c("wesley", 1.82, "jesse", 1.92,
              "poli", 1.61, "isa", 1.68,
              "pedro", 1.62)
is.numeric(nome.alt)
length(nome.alt)
nome.alt[c(7, 8, 3, 4)]
nome.alt[7:8]

#matriz
m1 <- matrix(data = c(3,4,2,5,7,8,2,5,4), nrow = 3, ncol = 3)
colnames(m1) <- c("C1", "C2", "C3")
colSums(m1)
m1 * 2 
v1 <- c(2,3,4)
m1 * v1
m1 %*% v1
diag(m1)
solve(m1)

#array

ar1 <- array(1:24, dim = c(3, 4, 2))

#lista
l1 <- list(1, 2, 3)
list.nomes <- list(nome = nomes, alt = altura)
list.nomes$alt[3]


#dataframe
df1 <- data.frame(nome = nomes, alt = altura)
str(df1)
summary(df1)

df1$alt[5]
head(df1, 3)
tail(df1, 2)
names(df1)

#operadores logicos
2 == 2 #igual
2 > 3
2 < 3
2 >= 2
3 <= 3
3 != 3
(2 == 2) & (3 != 3)
(2 == 2) | (3 != 3)

#Start R
max(df1$alt)

cv.alt <- sd(df1$alt)/mean(df1$alt) * 100
round(cv.alt, 2)
ceiling(cv.alt)
rank(df1$alt)


order(df1$alt)

df1[order(df1$alt, decreasing = T),]

#lendo arquivo de dados

dados <- read.csv("dataset/base_dados.csv", header = T, 
                  sep = ",", dec = ".")
head(dados)
tail(dados)
str(dados)

dados$tratamento <- paste(dados$espacamento, dados$clone,
                          sep = "")
head(dados)
write.csv(dados, "dataset/nova_base_dados.csv", row.names = F)

#Apply
apply(dados[,5:6], 2, mean)

tapply(dados$altura, dados$espacamento, mean)

#dplyr
library(dplyr)

df.e1 <- filter(dados, (espacamento == "E1"))
df.e1e4 <- filter(dados, ((espacamento == "E1") | (espacamento == "E4")))

df.e2c2 <- dados %>% 
    filter(espacamento == "E2") %>% 
    filter(clone == "C2")

df2 <- dados %>% 
    select(espacamento, clone, altura, dap)
head(df2)

#mutate
df3 <- dados %>% 
    mutate(volume = dap^2*pi/40000*altura*0.51)
head(df3)

#uso da funcao arrange
View(dados)

df4 <- dados %>% 
    arrange(arvoresh) 

df4.1 <- df4 %>% 
    arrange(desc(dap))

head(df4.1)
View(df4.1)

#summarise
df5 <- dados %>% 
    group_by(espacamento, clone) %>% 
    summarise(arvoresh = unique(arvoresh), med_alt = mean(altura), 
              med_dap = mean(dap), sd_alt = sd(altura), sd_dap = sd(dap))

sink("dataset/resumo_estatistico.doc")
df5
sink()

View(df5)

#tidyr
library(tidyr)

#spread
df6 <- df5 %>% 
    select(espacamento, clone, med_alt) %>% 
    spread(clone, med_alt)
View(df6)

#gather 
df7 <- df6 %>% 
    gather(clone, med_alt, 2:3)

View(df7)

#programacao

a <- 10
b <- 5

if(a > b) print("Hello World!")

if(a <= b) {
    print("Continuar aula...")
}else{
    print("Vamos coffe brake...")
}

#while
a <- 0

while(a <= 10){
    b <- a * 2
    print(b)
    a <- a + 1
}

x <- 1
while(x != 0){
    print("Digite um valor!")
    x <- scan()
    print(paste("Você digitou o valor: ", x))
    print("Digite o valor 0 para sair!")
}
names(dados)

dados$tratamento <- paste(dados$espacamento, dados$clone, sep = "")
dados$tratamento <- as.factor(dados$tratamento)
for(i in c(5, 6)){
    variavel <- dados[, i]
    trat <- dados[, 7]
    aov_var <- aov(variavel ~ trat)
    var_anova <- anova(aov_var)
    print(var_anova)
}
head(dados)
str(dados)

a <- 1:10
for(k in a){
    log_a <- log(k)
    print(log_a)
}

#funcoes

#coeficiente de variacao 
cv <- function(x){
    # x e um vetor numerico
    cv <- sd(x)/mean(x) * 100
    return(cv)
}
cv(dados$altura)    

calc_vol <- function(alt, dap, ff=0.51){
    vol <- dap^2*pi/40000*alt*ff
    return(vol)
}
calc_vol(15, 12, 0.65)
calc_vol(dados$altura, dados$dap, 0.65)


