#CNN that needs PIL
library(tuneR)
library(signal, warn.conflicts = F, quietly = T)
library(oce, warn.conflicts = F, quietly = T)
library(png)
library(tensorflow)
library(magrittr)
library(imager)
library(EBImage)
library(keras)
install_keras()
#making training dataset
dir.create("~/Desktop/Temporary")
dir.create("~/Desktop/Temporary/Train")
dir.create("~/Desktop/Temporary/Test")
dir.create("~/Desktop/Train")
dir.create("~/Desktop/Test")
dir.create("~/Desktop/Train/Train_Normal")
dir.create("~/Desktop/Train/Train_Abnormal")
dir.create("~/Desktop/Test/Test_Normal")
dir.create("~/Desktop/Test/Test_Abnormal")
train_label <- list()
x <- 99
for(val in 1:x){
  fnumber <- 0
  if(val/10 < 1){
    fnumber <- paste0("000", val)
  } else if((val/10 >= 1)&(val/100 < 1)){
    fnumber <- paste0("00", val)
  } else if(val/100 >= 1){
    fnumber <- paste0("0", val)
  }
  if(file.exists(paste0("~/Desktop/Normal/a", fnumber, ".wav"))){
    png(filename = paste0("~/Desktop/Temporary/Train/a", fnumber, ".png"))
    wav <- readWave(paste0("~/Desktop/Normal/a", fnumber, ".wav"))
    values <- (wav@left)/2^(wav@bit -1)
    matrix_values <- as.matrix(values)
    time <- (0:11999)/wav@samp.rate
    waveplot <- plot(time, matrix_values[1:12000,], type='l', col='black', xlab='Time(s)', ylab='Amplitude')
    window_length <- 150
    overlap_value <- 100
    fs <- wav@samp.rate
    spec <- specgram(x=matrix_values[1:12000,], n=150, Fs=fs, window=window_length, overlap=overlap_value)
    P = abs(spec$S)
    P = P/max(P)
    P = 10*log10(P)
    image <- imagep(x=spec$t, y=spec$f, z=t(P), axes=F, col=oce.colorsViridis, drawPalette=F, decimate=F)
    dev.off()
    image <- readImage(paste0("~/Desktop/Temporary/Train/a", fnumber, ".png"))
    greyimage <- channel(image, "grey")
    noise <- array(runif(480*480*1*1),c(480*480*1*1))
    write.csv(noise, paste0("~/Desktop/Train/Train_Normal/a", fnumber, ".csv"), row.names = FALSE)
    train_label <- c(train_label, "Normal")
    next
  } else if(file.exists(paste0("~/Desktop/Abnormal/a", fnumber, ".wav"))){
    png(filename = paste0("~/Desktop/Temporary/Train/a", fnumber, ".png"))
    wav <- readWave(paste0("~/Desktop/Abnormal/a", fnumber, ".wav"))
    values <- (wav@left)/2^(wav@bit -1)
    matrix_values <- as.matrix(values)
    time <- (0:11999)/wav@samp.rate
    waveplot <- plot(time, matrix_values[1:12000,], type='l', col='black', xlab='Time(s)', ylab='Amplitude')
    window_length <- 150
    overlap_value <- 100
    fs <- wav@samp.rate
    spec <- specgram(x=matrix_values[1:12000,], n=150, Fs=fs, window=window_length, overlap=overlap_value)
    P = abs(spec$S)
    P = P/max(P)
    P = 10*log10(P)
    image <- imagep(x=spec$t, y=spec$f, z=t(P), axes=F, col=oce.colorsViridis, drawPalette=F, decimate=F)
    dev.off()
    image <- readImage(paste0("~/Desktop/Temporary/Train/a", fnumber, ".png"))
    greyimage <- channel(image, "grey")
    noise <- array(runif(480*480*1*1),c(480*480*1*1))
    write.csv(noise, paste0("~/Desktop/Train/Train_Abnormal/a", fnumber, ".csv"), row.names = FALSE)
    train_label <- c(train_label, "Abnormal")
    next
  }else{
    next
  }
}
train_dir <- "~/Desktop/Train"
#making test dataset
x <- 200
for(val in 100:x){
  fnumber <- paste0("0", val)
  if(file.exists(paste0("~/Desktop/Normal/a", fnumber, ".wav"))){
    png(filename = paste0("~/Desktop/Temporary/Test/a", fnumber, ".png"))
    wav <- readWave(paste0("~/Desktop/Normal/a", fnumber, ".wav"))
    values <- (wav@left)/2^(wav@bit -1)
    matrix_values <- as.matrix(values)
    time <- (0:11999)/wav@samp.rate
    waveplot <- plot(time, matrix_values[1:12000,], type='l', col='black', xlab='Time(s)', ylab='Amplitude')
    window_length <- 150
    overlap_value <- 100
    fs <- wav@samp.rate
    spec <- specgram(x=matrix_values[1:12000,], n=150, Fs=fs, window=window_length, overlap=overlap_value)
    P = abs(spec$S)
    P = P/max(P)
    P = 10*log10(P)
    image <- imagep(x=spec$t, y=spec$f, z=t(P), axes=F, col=oce.colorsViridis, drawPalette=F, decimate=F)
    dev.off()
    image <- readImage(paste0("~/Desktop/Temporary/Test/a", fnumber, ".png"))
    greyimage <- channel(image, "grey")
    noise <- array(runif(480*480*1*1),c(480*480*1*1))
    write.csv(noise, paste0("~/Desktop/Test/Test_Normal/a", fnumber, ".csv"), row.names = FALSE)
  } else if(file.exists(paste0("~/Desktop/Abnormal/a", fnumber, ".wav"))){
    png(filename = paste0("~/Desktop/Temporary/Test/a", fnumber, ".png"))
    wav <- readWave(paste0("~/Desktop/Abnormal/a", fnumber, ".wav"))
    values <- (wav@left)/2^(wav@bit -1)
    matrix_values <- as.matrix(values)
    time <- (0:11999)/wav@samp.rate
    waveplot <- plot(time, matrix_values[1:12000,], type='l', col='black', xlab='Time(s)', ylab='Amplitude')
    window_length <- 150
    overlap_value <- 100
    fs <- wav@samp.rate
    spec <- specgram(x=matrix_values[1:12000,], n=150, Fs=fs, window=window_length, overlap=overlap_value)
    P = abs(spec$S)
    P = P/max(P)
    P = 10*log10(P)
    image <- imagep(x=spec$t, y=spec$f, z=t(P), axes=F, col=oce.colorsViridis, drawPalette=F, decimate=F)
    dev.off()
    image <- readImage(paste0("~/Desktop/Temporary/Test/a", fnumber, ".png"))
    greyimage <- channel(image, "grey")
    noise <- array(runif(480*480*1*1),c(480*480*1*1))
    write.csv(noise, paste0("~/Desktop/Test/Test_Abnormal/a", fnumber, ".csv"), row.names = FALSE)
  }
}
test_dir <- "~/Desktop/Test"
#training using directory
#convolutional base
model <- keras_model_sequential() %>%
  layer_conv_2d(filters=32, kernel_size=c(3,3), activation='relu', input_shape= c(480,480,4)) %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_conv_2d(filters=64, kernel_size=c(3,3), activation='relu') %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_conv_2d(filters=64, kernel_size=c(3,3), activation='relu')
summary(model)
#add dense layers on top
model %>%
  layer_flatten() %>%
  layer_dense(units=64, activation='relu') %>%
  layer_dense(units=10, activation='softmax')
summary(model)
#datagen
datagen = image_data_generator(
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest",
  samplewise_std_normalization = TRUE
)
#image generator setup for training
train_generator=flow_images_from_directory(
  train_dir,
  datagen,
  color_mode='rgba',
  target_size=c(480,480),
  classes=c('Train_Normal','Train_Abnormal'),
  class_mode='categorical',
  shuffle=TRUE
)
test_generator=flow_images_from_directory(
  test_dir,
  datagen,
  color_mode='rgba',
  target_size=c(480,480),
  classes=c('Test_Normal','Test_Abnormal'),
  class_mode='categorical',
  shuffle=TRUE
)
#compile and train model
model %>% compile(
  optimizer='adam',
  loss='sparse_categorical_crossentropy',
  metrics='accuracy'
)
history <- model %>%
  fit_generator(
    train_dir,
    steps_per_epoch=67,
    epochs=1,
    validation_data=train_dir,
    verbose=1
  )
plot(history)
evaluate(model,train_generator, verbose = 0)
