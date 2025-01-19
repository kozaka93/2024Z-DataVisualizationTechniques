## Nawigacja pomiędzy dwoma punktami w miastach (skrzyżowaniami)

Zainspirowałem się tym filmem na yt:

[Ładna wizualizacja algorytmu A* w kilku miastach](https://www.youtube.com/watch?v=CgW0HPHqFE8)

Kiedyś myślałem, aby stworzyć podobną rzecz i użyć jako wygaszacza ekranu, ale nigdy się tym nie zająłem aż do teraz.

Jednak jak podszedłem do problemu to okazało się to nieco trudniejsze niż myślałem, zwłaszcza aby nie obciążało to zbytnio procesora oraz wyglądało w miarę przyzwoicie. W filmie zastosowany został skrypt pythonowy w blenderze, prawdopodobnie dlatego uzyskano tak ładne efekty.

Ostatecznie zdecydowałem się na statyczną wizualizację, nadal z wyborem miasta oraz nawigacją pomiędzy dwoma losowo wybranymi punktami w tym mieście. Do nawigacji użyty został algorytm A*, chociaż nie jestem do końca przekonany patrząc na jego wyniki czy na pewno jest poprawnie zaimplementowany. Znajduje najkrótszą ścieżkę, ale przegląda podejrzanie dużą część grafu.

Na wizualizacji przedstawiam wszystkie odwiedzone krawędzie grafu na niebiesko, na żółto najkrótszą ścieżkę, oraz początek i koniec odpowiednio na zielono i czerwono.