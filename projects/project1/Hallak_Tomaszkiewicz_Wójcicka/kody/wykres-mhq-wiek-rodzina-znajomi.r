library(ggplot2)

rodzina <- read.csv("dane-rodzina-csv.csv", sep = ';', col_types = cols())
znajomi <- read.csv("dane-znajomi-csv.csv", sep = ';', col_types = cols())

ggplot(rodzina, aes(x = wiek)) +
  geom_point(aes(y = X1, color = "Relacja 1", group = 1)) +
  geom_point(aes(y = X2, color = "Relacja 2", group = 2)) +
  geom_point(aes(y = X3, color = "Relacja 3", group = 3)) +
  geom_point(aes(y = X4, color = "Relacja 4", group = 4)) +
  geom_point(aes(y = X5, color = "Relacja 5", group = 5)) +
  labs(title = "Wynik MHQ w zależności od grupy wiekowej (Rodzina)",
       x = "Grupa wiekowa",
       y = "Wynik MHQ",
       color = "Relacja z rodziną") +
  theme_minimal()


ggplot(znajomi, aes(x = wiek)) +
  geom_line(aes(y = X0, color = "Znajomi 0", group = 1)) +
  geom_line(aes(y = `od.1.do.3`, color = "Znajomi 1-3", group = 2)) +
  geom_line(aes(y = `od.4.do.6`, color = "Znajomi 4-6", group = 3)) +
  geom_line(aes(y = `od.7.do.9`, color = "Znajomi 7-9", group = 4)) +
  geom_line(aes(y = X10., color = "Znajomi 10+", group = 5)) +
  labs(title = "Wynik MHQ w zależności od grupy wiekowej (Znajomi)",
       x = "Grupa wiekowa",
       y = "Wynik MHQ",
       color = "Liczba bliskich znajomych") +
  theme_minimal()


