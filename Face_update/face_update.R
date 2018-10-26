parse_website <- function(url){
  require(xml2)
  pag <- read_html(url)
  fs <- xml_find_all(pag,xpath = "//h3[@class= 'item-title']/a")
  ss <- xml_attr(fs,"href")
  return(ss)
}

checkBlog <- function(newList,oldList,token){
  require(Rfacebook)
  teste <- prod(newList == oldList)
  if(teste==1){
    return("No updates")
  } else{
    readline(prompt= "New post. Press [enter] to continue")
    new_post_url <- newList[newList != oldList][1]
    new_post_url <- paste0("https://azul.netlify.com/",new_post_url)
    updateStatus("Novo post", token = token, link = new_post_url)
  }
}

save(obj, file = "Face_update/Lista.Rdata")
