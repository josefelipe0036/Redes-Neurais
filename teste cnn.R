pacman::p_load(tidyverse, keras, tensorflow, reticulate)
# Definir as dimensões das imagens de entrada
input_shape <- c(28, 28, 1)  # Tamanho da imagem: 28x28, 1 canal (grayscale)
modelo <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = input_shape) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# Compilar o modelo
modelo %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)
caminho_pasta <- "/home/jose/UnB/Redes Neurais/trabalho/Pasta sem título/Descompactados" #fotos
caminho_pasta<-"/home/jose/pasta_destino/teste" #fotos somente bordas 
# Pré-processamento das imagens
# Redimensionar, normalizar e converter as imagens em arrays
imagens <- list.files(caminho_pasta, full.names = TRUE)
imagens <- imagens[1:1200] 
# Selecionar apenas as 50 primeiras imagens para o exemplo
dados <- lapply(nomes_arquivos_filtrados, function(path) {
  img <- image_load(path, target_size = c(28, 28), grayscale = TRUE)
  img <- image_to_array(img)
  img <- img / 255
  return(img)
})
dados <- array_reshape(dados, c(length(dados), 28, 28, 1))







# Treinar o modelo
labels <- matrix(0, nrow = length(dados), ncol = 10)
history <- modelo %>% fit(
  x = dados,
  y = labels,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2
)








get_embeddings <- keras_model(inputs = modelo$input, outputs = modelo$layers[[5]]$output)
embeddings <- predict(get_embeddings, dados)

library(stats)
num_clusters <- 8 # Número de clusters desejado
clusters <- kmeans(embeddings, centers = num_clusters)
