  # Instalacja i ładowanie pakietów
# Importowanie pakietu
library(wordcloud2)
library(htmlwidgets)

# Dane
reasons <- c(
  "No commitment", "Affairs", 
  "Arguing", "Married too young", 
  "Financial problems", "Substance abuse", "Domestic violence", 
  "Health problems", "No family support", "Religious", 
  "Little education"
)
values <- c(75.0, 59.6, 57.7, 45.1, 36.7, 34.6, 23.5, 18.2, 17.3, 13.3, 13.3)

# Tworzenie ramki danych
data <- data.frame(word = reasons, freq = values)

# Nowa paleta kolorów (jasne tony niebieskiego, białego, żółtego)
my_palette <- c("blue", "lightblue", "yellow",
                "blue", "lightblue","blue", "yellow",
                "blue", "lightblue", "yellow","blue")

set.seed(123234)

# Generowanie chmury słów

# Generowanie chmury słów (przypisanie do obiektu)
wordcloud <- wordcloud2(
  data = data,
  size = 0.5,                # Skala całego wykresu
  color = my_palette,        # Paleta kolorów dla słów
  rotateRatio = 0,           # Brak rotacji - wszystkie słowa poziomo
  backgroundColor = "rgba(0,0,0,0)"  # Przezroczyste tło (w formacie RGBA)
)
wordcloud
# Zapisywanie chmury słów do pliku HTML
saveWidget(wordcloud, "chmura_slow.html", selfcontained = TRUE)

# # Instalacja dodatkowego pakietu, jeśli nie jest jeszcze zainstalowany
# if (!require("webshot")) install.packages("webshot")
# webshot::install_phantomjs()

# Zapisywanie chmury słów jako pliku PNG
webshot::webshot(
  url = "chmura_slow.html", 
  file = "chmura_slow.png", 
  vwidth = 800, 
  vheight = 600,
  cliprect = "viewport",   # Wycina tylko obszar widoku
  delay = 0.5,             # Czas na załadowanie widoku
  selector = "body"        # Zapewnia, że renderuje poprawny element
)


  
  
