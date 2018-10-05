setwd("C:/Users/pedro/Downloads/AC2017")
library(readr)

link = "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2017/"

dados <- read_delim(file = "AC2017.txt",
                    delim = ";", escape_double = FALSE, 
                     trim_ws = TRUE,
                    locale = locale(encoding = "ISO-8859-1"))
View(dados)

library(dplyr)
dados$`Bairros SP` = NULL
dados$`Bairros Fortaleza` = NULL
dados$`Bairros RJ` = NULL
dados$`Distritos SP` = NULL
dados$`Regiões Adm DF` = NULL

dados$`Vl Rem Janeiro CC` = NULL
dados$`Vl Rem Fevereiro CC` = NULL
dados$`Vl Rem Março CC`= NULL
dados$`Vl Rem Abril CC`= NULL
dados$`Vl Rem Maio CC`= NULL
dados$`Vl Rem Junho CC`= NULL
dados$`Vl Rem Julho CC`= NULL
dados$`Vl Rem Agosto CC`= NULL
dados$`Vl Rem Setembro CC`= NULL
dados$`Vl Rem Outubro CC`= NULL
dados$`Vl Rem Novembro CC`= NULL

dados$`Faixa Etária` = NULL
dados$`Tipo Estab` = NULL
dados$`Causa Afastamento 1` = NULL
dados$`Causa Afastamento 2` = NULL
dados$`Causa Afastamento 3` = NULL

##

dados$aposentadoria = ifelse(dados$`Motivo Desligamento` > 65, 1, 0)
dados$morte = ifelse(dados$`Motivo Desligamento` > 59 & dados$`Motivo Desligamento` < 69, 1, 0)
dados$justacausa = ifelse(dados$`Motivo Desligamento` == 10, 1, 0)
dados$outrosmotivos_termino = ifelse(dados$aposentadoria == 0 & dados$justacausa == 0 & dados$morte == 0, 1, 0)

dados$demissao = ifelse(dados$`Motivo Desligamento` == 0 | dados$`Motivo Desligamento` == -1, 0, 1)

dados$`Faixa Hora Contrat` = NULL
dados = rename(dados, horas = `Qtd Hora Contr`)
dados$gringo = ifelse(dados$Nacionalidade == 10, 0, 1)
dados$Nacionalidade = NULL

dados$branco = ifelse(dados$`Raça Cor` == "02", 1, 0)
dados$negro = ifelse(dados$`Raça Cor` == "04" | dados$`Raça Cor` == "08", 1, 0)
dados$outra_etnia = ifelse(dados$branco == 0 & dados$negro == 0, 1, 0)
dados$etnia = ifelse(dados$branco == 1, "Branco", 
         ifelse(dados$negro == 1, "Negro", "Outras etnias"))

dados$homem = ifelse(dados$`Sexo Trabalhador` == "01", 1, 0)
dados$sexo = ifelse(dados$homem == 1, "Masculino", "Feminino")
dados$`Sexo Trabalhador` = NULL
dados$`Raça Cor` = NULL

dados = rename(dados, duracao = "Tempo Emprego")
dados$CNPJ = ifelse(dados$`Tipo Estab_1` == "CNPJ", 1, 0)
dados$ensino_superior = ifelse(as.numeric(dados$`Escolaridade após 2005`) > 8, 1, 0)
dados$`Escolaridade após 2005` = NULL

dados$firma_grande = ifelse(as.numeric(dados$`Tamanho Estabelecimento`) > 7, 1, 0)
dados$`Tamanho Estabelecimento` = NULL

dados$industria = ifelse(as.numeric(dados$`IBGE Subsetor`) < 15, 1, 0)
dados$servicos = ifelse(as.numeric(dados$`IBGE Subsetor`) > 15 & as.numeric(dados$`IBGE Subsetor`) < 25, 1, 0)
dados$agricultura = ifelse(as.numeric(dados$`IBGE Subsetor`) == 25, 1, 0)
dados$construcao = ifelse(as.numeric(dados$`IBGE Subsetor`) == 15, 1, 0)

dados$`IBGE Subsetor` = NULL

dados$setor = ifelse(dados$industria == 1, "Indústria",
                     ifelse(dados$servicos == 1, "Comércio e Serviços",
                            ifelse(dados$construcao == 1, "Construção", 
                                   ifelse(dados$agricultura == 1, "Agricultura", "Outros"))))

setwd("C:/Users/pedro/Desktop/azul/content/post/cox_rais")
saveRDS(dados, file = "acre_rais_2017.Rds")


