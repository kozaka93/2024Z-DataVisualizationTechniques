library(ggplot2)

# Tworzymy dane dla zarysu choinki z 5 poziomami
tree_outline <- data.frame(
  x = c(
    0, -1.5, -2, -3, -2.5, -4, 0, 4, 2.5, 3, 2, 1.5, 0,   # Poziom 1
    0, -2, -2.5, -4, -3.5, -5, 0, 5, 3.5, 4, 2.5, 2, 0,   # Poziom 2
    0, -3, -3.5, -5, -4.5, -6, 0, 6, 4.5, 5, 3.5, 3, 0,   # Poziom 3
    0, -4, -4.5, -6, -5.5, -7, 0, 7, 5.5, 6, 4.5, 4, 0,   # Poziom 4
    0, -5, -5.5, -7, -6.5, -8, 0, 8, 6.5, 7, 5.5, 5, 0    # Poziom 5
  ),
  y = c(
    0, -1, -1.5, -2, -2.5, -3, -4, -3, -2.5, -2, -1.5, -1, 0,   # Poziom 1
    -4, -5, -5.5, -6, -6.5, -7, -8, -7, -6.5, -6, -5.5, -5, -4,  # Poziom 2
    -8, -9, -9.5, -10, -10.5, -11, -12, -11, -10.5, -10, -9.5, -9, -8, # Poziom 3
    -12, -13, -13.5, -14, -14.5, -15, -16, -15, -14.5, -14, -13.5, -13, -12, # Poziom 4
    -16, -17, -17.5, -18, -18.5, -19, -20, -19, -18.5, -18, -17.5, -17, -16  # Poziom 5
  ),
  group = rep(1:5, each = 13)
)

# Tworzymy dane dla gwiazdki (umieszczonej wyżej)
star <- data.frame(
  x = c(0),
  y = c(-0.5),  # Umieszczamy gwiazdkę wyżej, tuż nad czubkiem choinki
  label = c("*")
)

# Tworzymy pień choinki
tree_trunk <- data.frame(
  x = c(-1, -1, 1, 1),
  y = c(-20, -19, -19, -20)
)
ornaments <- data.frame(
  x = c(0.5, -1.2, 1.5, -2.5, 2.5, 0, -3.5, 2, -4, 3, -0.5, 4, 1, -1,-1,-1, -4, 2),  # Współrzędne x dla bombek
  y = c(-3, -5.4, -5.8, -7, -6.8, -9.5, -11, -9.8, -14, -14.5, -18,-17.5, -13.5, -14.8,-11, -2,-18.5, -19),  # Współrzędne y dla bombek
  color = c("red", "blue", "gold", "grey", "red", "blue", "gold", "yellow", "red", "blue", "gold","magenta","orange","yellow","brown","black","navy","red")  # Kolory bombek
)

# Tworzymy wykres z bombkami
ggplot() +
  # Dodajemy kontur choinki z wypełnieniem zielonym
  geom_polygon(data = tree_outline, aes(x = x, y = y, group = group), fill = "green", color = "black", size = 1) +
  # Dodajemy gwiazdkę (złotą) na czubek choinki
  geom_text(data = star, aes(x = x, y = y, label = label), size = 30, color = "gold") +
  # Dodajemy pień choinki
  geom_polygon(data = tree_trunk, aes(x = x, y = y), fill = "saddlebrown", color = "black") +
  # Dodajemy bombki
  geom_point(data = ornaments, aes(x = x, y = y, color = color), size = 5, shape = 16) +
  # Ustawiamy kolory bombek
  scale_color_identity() +
  # Dostosowujemy wygląd wykresu
  theme_void() +
  coord_equal()

