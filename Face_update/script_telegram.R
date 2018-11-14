library(telegram.bot)

rm(list=ls())

source("Face_update/func_telegram.R")

load(paste0(getwd(),"/Face_update/dados.Rdata"))
load(paste0(getwd(),"/Face_update/Lista.Rdata"))

site <- parse_website("https://azul.netlify.com/")
post <- checkBlog(site,obj)

bot <- Bot(token = bot_token('azulblog_bot'))
updates <- bot$getUpdates()

novos_usuarios <- get_new_users(updates)

novos_usu <- check_new_users(dados,novos_usuarios)

dados <- rbind(dados,novos_usu)

del_users <- cancel_users(dados,updates)

cancel_users_msg(del_users)

answer_new_users(novos_usu)

dados <- dados[-(dados[,2] == del_users),]

save(dados,file = paste0(getwd(),"/Face_update/dados.Rdata"))

obj <- site

save(obj, file = paste0(getwd(),"/Face_update/Lista.Rdata"))
