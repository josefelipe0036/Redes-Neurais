library(rvest)

# URL do site que contém os links para os arquivos RAR
url <- "http://mapas.segeth.df.gov.br/"

download_file <- function(url, dest_dir) {
  download.file(url, dest_dir, mode = "wb")
}

download_file <- function(url, dest_file) {
  tryCatch(
    expr = {
      download.file(url, destfile = dest_file, mode = "wb")
    },
    error = function(e) {
      # Exibir mensagem de erro ou ação alternativa, se necessário
      message(paste("Erro ao baixar o arquivo:", url))
    }
  )
}

# Realizar o scraping da página
page <- read_html(url)

# Extrair os links para os arquivos .rar
links <- page %>% 
  html_nodes("a[href$='.rar']") %>% 
  html_attr("href")

dest_dir <- "/home/jose/UnB/Redes Neurais/Pasta sem título/"
for (link in links) {
  file_url <- paste0(url, links)
}

a=as.data.frame(file_url)

teste <- function() {
  
for (i in 1:length(a$file_url)) {
  dest_file <- paste0(dest_dir, "arquivo", i, ".rar")
  download_file(a$file_url[i], dest_file)
}

}