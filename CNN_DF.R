library(keras)

# Definir o caminho para as imagens
caminho_pasta <- "/home/jose/UnB/Redes Neurais/trabalho/Pasta sem título/Descompactados"

# Filtrar os arquivos por nome
imagens <- list.files(caminho_pasta, full.names = TRUE)

# Filtrar apenas as imagens que começam com 1964 1978 e 1997
nomes_arquivos_filtrados <- grep("^/home/jose/UnB/Redes Neurais/trabalho/Pasta sem título/Descompactados/1997", imagens, value = TRUE)

# Carregar e redimensionar as imagens
imagens <- list.files(caminho_pasta, full.names = TRUE)
# Selecionar apenas as 50 primeiras imagens para o exemplo
dados <- lapply(nomes_arquivos_filtrados, function(path) {
  img <- image_load(path, target_size = c(28, 28), grayscale = TRUE)
  img <- image_to_array(img)
  img <- img / 255
  return(img)
})

##caso houver erro

dados <- lapply(nomes_arquivos_filtrados, function(path) {
  tryCatch({
    img <- image_load(path, target_size = c(28, 28), grayscale = TRUE)
    img <- image_to_array(img)
    img <- img / 255
    return(img)
  }, error = function(e) {
    # Tratamento de exceção para imagens truncadas
    warning(paste("Erro ao processar imagem:", path))
    NULL  # Retorna NULL para indicar que houve um erro
  })
})

dados <- array_reshape(dados, c(length(dados), 28, 28, 1))



##dados de 97
dados <- lapply(nomes_arquivos_filtrados, function(path) {
  tryCatch({
    img <- image_load(path, target_size = c(28, 28), grayscale = TRUE)
    img <- image_to_array(img)
    img <- img / 255
    return(img)
  }, error = function(e) {
    return(NULL)  # Retorna NULL em caso de erro
  })
})

# Filtrar as imagens válidas
dados <- Filter(Negate(is.null), dados)

# Converter a lista de arrays em uma matriz
dados <- array_reshape(dados, c(length(dados), 28, 28, 1))


# Verificar as dimensões das imagens redimensionadas
print(dim(dados))

modelo_carregado <- load_model_hdf5("/home/jose/UnB/Redes Neurais/trabalho/modelo.h5")
previsoes <- predict(modelo_carregado, dados)

# Converter as previsões em classes
classes_preditas <- previsoes %>% k_argmax()

classes_preditas=as.integer(classes_preditas)

df_resultado <- data.frame(Caminho = nomes_arquivos_filtrados, Classe_Prevista = classes_preditas)

######dados de 97
# Verificar o comprimento mínimo entre os nomes de arquivos filtrados e os dados carregados
comprimento_minimo <- min(length(nomes_arquivos_filtrados), length(dados))

# Criar o data.frame com o comprimento mínimo
df_resultado <- data.frame(Caminho = nomes_arquivos_filtrados[1:comprimento_minimo], Classe_Prevista = classes_preditas[1:comprimento_minimo])


# Exibir o dataframe
print(df_resultado)


# Criar as pastas para cada classe
pasta_destino <- "/home/jose/UnB/Redes Neurais/trabalho/Classe_97"
for (classe in unique(classes_preditas)) {
  caminho_pasta_classe <- file.path(pasta_destino, paste0("Classe_", classe))
  dir.create(caminho_pasta_classe, showWarnings = T)
}

# Copiar as imagens para as pastas de destino
for (i in 1:length(nomes_arquivos_filtrados)) {
  caminho_origem <- nomes_arquivos_filtrados[i]
  classe <- classes_preditas[i]
  caminho_destino <- file.path(pasta_destino, paste0("Classe_", classe), basename(caminho_origem))
  file.copy(caminho_origem, caminho_destino)
}

