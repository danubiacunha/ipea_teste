
###############################################################################
## "Atividade IPEA - Edital 04/2022"                                         ## 
##  Data: 03-março-2022                                                      ## 
##  Autora: Danúbia Rodrigues da Cunha                                       ##                                                ##
###############################################################################
## Arquivo: leitura_ipea_dados.R                                             ##
###############################################################################

###############################################################################
## Pacote utilizado
###############################################################################

library(tidyverse) 
library(cowplot)

###############################################################################
## Leitura do banco de dados csv
###############################################################################

dados = read.csv("C:/Users/danub/Desktop/Teste - IPEA/simulacao_ipea.csv", header = TRUE, sep = ";")

head(dados) ## mostra as primeiras entradas
dim(dados) # mostra as dimensões de "dados"


###############################################################################
## Questão 4: Qual é a quantidade de trabalhadores do sexo masculino com idade 
## igual ou superior a 50 anos, registrados no estado da Bahia, no ano de 2019?
###############################################################################


dadosQ4 <- filter(dados, sexo=="M" & idade >=50 & uf == "BA" & ano == 2019)

head(dadosQ4)
dim(dadosQ4)

# Resposta = 3184

###############################################################################
## Questão 5: Qual ano e estabelecimento apresenta a maior quantidade de 
## registros do sexo feminino? 
###############################################################################

dadosQ5 <- filter(dados, sexo == "F")
head(dadosQ5)

freqAno <- table(dadosQ5$ano)
max(freqAno)

which(freqAno == max(freqAno))

# Resposta = ano 2017

freqOrgao <- table(dadosQ5$orgao)

max(freqOrgao)
which(freqOrgao == max(freqOrgao))


# Resposta = orgão ANA


###############################################################################
## Questão 6: Sabendo-se que o nível de escolaridade de um trabalhador vai 
## de 1 até 5, qual é o percentual de trabalhadores, registrados no estabecimento
## Ipea, com escolaridade nível 4 no ano de 2015?
###############################################################################

dadosQ6 <- filter(dados, orgao == "Ipea" & ano == 2015)
head(dadosQ6)
dim(dadosQ6)

sum(dadosQ6$escolaridade == 4, na.rm = TRUE)

round(448/1118*100,1)

# Resposta = 40,1 %


###############################################################################
## Questão 7: Ao analisar os números de mulheres registradas no estabelecimento 
## UFSJ, apenas para o ano de 2010, qual raça apresenta a menor quantidade de 
## registros? Obs. desconsiderar sem a raça declarada.
###############################################################################

dadosQ7 <- filter(dados, sexo == "F" & orgao == "UFSJ" & ano == 2010)
head(dadosQ7)

table(dadosQ7$raca)

## Resposta = Amarelo


###############################################################################
## Questão 8: Carregar um gráfico, preferencialmente nos formatos HTML, PNG, 
## JPEG, e PDF, contendo uma série temporal de 1985 até 2019, que trate sobre 
## a diferença de sexo e raça. Você pode explorar as variáveis que achar mais 
## relevantes.
###############################################################################

## Obs.: a série não começa em 1985, mas sim, em 1990. 

p1 <- ggplot(dados, mapping = aes(x = ano, y = remuneracao, colour = raca)) + 
      geom_smooth(method="lm", se=FALSE ) + 
      labs(x = "Ano", y = "Remuneração") + 
      facet_wrap(~sexo)

p2 <- ggplot(dados, mapping = aes(x = ano, y = escolaridade, colour = raca)) + 
  geom_smooth(method="lm", se=FALSE ) + 
  labs(x = "Ano", y = "Escolaridade") + 
  facet_wrap(~sexo)

plot_grid(p1,p2,
          labels = c("A","B"),
          ncol=1,
          nrow=2)



