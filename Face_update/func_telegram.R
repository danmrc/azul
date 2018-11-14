get_new_users <- function(msgs){
  mensagens <- msgs$message$text
  selec <- mensagens == "/getupdate"
  dados_id <- msgs$message$from$id[selec]
  dados_nome <- msgs$message$from$first_name[selec]
  dados <- cbind(dados_nome,dados_id)
  return(dados)
}

check_new_users <- function(base,novos_dados){
  novos <- NULL
  for(j in 1:nrow(novos_dados)){
    checagem <- base[,2] == novos_dados[j,2]
    if(sum(checagem) == 0){
      novos <- rbind(novos,novos_dados[j,])
    }
  }
  return(novos)
}

answer_new_users <- function(data){
  if(is.null(nrow(data)) == T){
    Sys.sleep(1+rexp(1,rate = 1/2))
    msg = sprintf(paste0("Obrigado pelo interesse, ",data[1],". Agora você receberá atualizações automaticamente do AZUL!\n Se você não quiser mais receber updates, digite Stop"))
    bot$sendMessage(chat_id = data[2], text = msg)
  }
   else {
    tam <- nrow(data)
    for(i in 1:tam){
      Sys.sleep(1+rexp(1,rate = 1/2))
      msg = sprintf(paste0("Obrigado pelo interesse, ",data[i,1],". Agora você receberá atualizações automaticamente do AZUL!\n Se você não quiser mais receber updates, digite /stop"))
      bot$sendMessage(chat_id = data[i,2], text = msg)
    }
  }
}

cancel_users <- function(base,dados){
  mensagens <- dados$message$text
  selec <- mensagens == "/stop"
  ids <- dados$message$from$id[selec]
  if(length(ids) == 0){
    stop("Ninguém quer ir embora!")
  }
  teste <- rep(0,length(ids))
  for(j in 1:length(ids)){
    teste[j] <- sum(base[,2] == ids[j])
  }
  if(sum(teste) == 0){
    stop("Ninguém cadastrado quer ir embora!")
  }
  return(ids)
}

cancel_users_msg <- function(id){
  msg <- sprintf("Você não receberá mais updates. Adeus e obrigado pelos peixes!")
  for(i in 1:length(ids)){
    Sys.sleep(1+rexp(1,rate = 1/2))
    bot$sendMessage(chat_id = ids[i],text = msg)
  }
  return("Feito")
}

parse_website <- function(url){
  require(xml2)
  pag <- read_html(url)
  fs <- xml_find_all(pag,xpath = "//h3[@class= 'item-title']/a")
  ss <- xml_attr(fs,"href")
  return(ss)
}

checkBlog <- function(newList,oldList){
  dife <- setdiff(newList,oldList)
  teste <- length(dife)
  if(teste==0){
    print("No updates")
  } else{
    readline(prompt = "New post. Press [enter] to continue")
    new_post_url <- dife[1]
    new_post_url <- paste0("https://azul.netlify.com/",new_post_url)
    return(new_post_url)
  }
}

send_post <- function(dados,url){
  msg <- paste0("Novo post no blog, acesse em ",url)
  for(j in 1:nrow(dados)){
    Sys.sleep(1+rexp(1,rate = 1/2))
    bot$sendMessage(chat_id = dados[j,2],text = msg)
  }
}
