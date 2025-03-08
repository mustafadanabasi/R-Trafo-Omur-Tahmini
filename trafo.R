# Gerekli kütüphaneler yükleniyor. Eğer yoksa yükleme yap.

if (!require("caret")) install.packages("caret")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("knitr")) install.packages("knitr")

library(readr)      # CSV dosyalarını okumak için
library(ggplot2)    # Grafik oluşturmak için
library(caret)      # Veri setini eğitim ve test olarak bölmek için
library(dplyr)      # Veri işleme işlemleri için
library(knitr)      # R Markdown veya HTML/PDF raporları oluşturmak için kullanılır.

# CSV dosyasını okuma
trafo_verileri <- read_csv("trafo_omur_tahmini.csv")

# Eksik değer kontrolü
sum(is.na(trafo_verileri))

# Verilerimiz içinde Omur_Tahmini_Yil değeri 0 dan küçük kayıtları temizliyoruz ve yeni bir değişkene atıyoruz.
trafo_verileri_temizlenmis <- trafo_verileri %>% filter(Omur_Tahmini_Yil > 0)
print(trafo_verileri_temizlenmis)

# İlk 10 satırı gösterme
head(trafo_verileri_temizlenmis, 10)

# Veri seti hakkında bilgi
str(trafo_verileri_temizlenmis)

# Satır ve sütun sayısı
dim(trafo_verileri_temizlenmis)

# Veri setini inceleyelim
head(trafo_verileri_temizlenmis)

# Veri seti özet istatistikleri
summary(trafo_verileri_temizlenmis)


# Veri yapısını inceleme
str(trafo_verileri_temizlenmis)

# Trafonun yaşı ile tahmini ömrü arasındaki ilişkisi  dağılım grafiği   
ggplot(trafo_verileri_temizlenmis, aes(x = Yas, y = Omur_Tahmini_Yil)) +
  geom_point(color = "blue", size = 3) +  # Mavi noktalar
  labs(title = "Trafo Yaş ve Ömür Yılı Dağılımı",
       x = "Trafo Yaşı (Yıl)",
       y = "Ömür Yılı (Yıl)") +
  theme_minimal()

# Yaş ve ömür arasındaki değişimi çizgi grafiği
ggplot(trafo_verileri_temizlenmis, aes(x = Yas, y = Omur_Tahmini_Yil)) +
  geom_line(color = "red", size = 1) +  # Kırmızı çizgi
  geom_point(color = "blue", size = 3) +  # Mavi noktalar
  labs(title = "Trafo Yaş ve Ömür Yılı Grafiği",
       x = "Trafo Yaşı (Yıl)",
       y = "Ömür Yılı (Yıl)") +
  theme_minimal()

# Yaş ve ömür arasındaki ilişki
ggplot(trafo_verileri_temizlenmis, aes(x = Yas, y = Omur_Tahmini_Yil)) +
  geom_point(aes(size = Yas, color = Yas), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  scale_color_gradient(low = "darkblue", high = "red") +
  labs(title = "Yaş ve Ömür arasındaki ilişki",
       x = "Trafo Yaşı (Yıl)",
       y = "Ömür Yılı (Yıl)",
       size = "Yas",
       color = "Yas") +
  theme_minimal()



# Linear regrasyon çalışmaları ile bir trafonun ömrünü tahminle


set.seed(1)  # Rastgelelik kontrolü
# %80 eğitim, %20 test veri seti olarak ayır
train_index <- createDataPartition(trafo_verileri_temizlenmis$Omur_Tahmini_Yil, p = 0.8, list = FALSE)

# Eğitim ve test veri setlerini oluştur
train_set <- trafo_verileri_temizlenmis[train_index, ]
test_set  <- trafo_verileri_temizlenmis[-train_index, ]

summary(train_set)
summary(test_set)
dim(train_set)
dim(test_set)

# Doğrusal regresyon modelini eğitim seti üzerinden oluştur
model <- train(Omur_Tahmini_Yil ~ Yas + Nem_Orani + Ariza_Gecmisi, data = train_set, method = "lm")

# Modelin özetini görüntüle
summary(model$finalModel)

# Modelin özetini görüntüle
summary(model)

# Model ile test seti üzerinde tahmin yap
tahminler <- predict(model, newdata = test_set)
print(tahminler)

# Gerçek değerler ve tahminleri karşılaştıran bir veri çerçevesi oluştur
sonuc <- data.frame(Gercek = test_set$Omur_Tahmini_Yil, Tahmin = tahminler)
print(sonuc)

# Mean Absolute Error (MAE) ve Root Mean Squared Error (RMSE) hesaplama
MAE <- mean(abs(sonuc$Gercek - sonuc$Tahmin))
RMSE <- sqrt(mean((sonuc$Gercek - sonuc$Tahmin)^2))

# Sonuçları ekrana yazdır
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")

#MAE = 2.59, yani modelin tahminleri ortalama 2.59 yıl sapma gösteriyor.
#RMSE = 3.13, yani modelde büyük hatalar varsa bunlar ortalama 3.13 yıl sapmaya neden oluyor.
#RMSE’nin MAE’den biraz büyük olması, modelde bazı büyük hatalar olduğunu gösteriyor ama aşırı büyük sapmalar olmadığını söyleyebiliriz.


ggplot(sonuc, aes(x = Gercek, y = Tahmin)) +
  geom_point(color = "blue", size = 3) +  # Mavi noktalar
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + # Y = X doğrusu
  labs(title = "Gerçek ve Tahmin Verilerinin Karşılaştırılması",
       x = "Gerçek Değerler",
       y = "Tahmin Edilen Değerler") +
  theme_minimal()


