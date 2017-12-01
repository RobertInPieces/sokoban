# sokoban-5

Implementacja sokobana w terminalu

1. Użyte symbole:
'#' - ściana
'@' - gracz
'*' - skrzynia
'.' - cel
' ' - wolne pole

Wyjaśnienie:
Użyte przeze mnie symbole częściowo nakładają się z symbolami z tabelki z wiki, poza kilkoma szczegółami
Znak skrzyni zmieniłem z '$' na '*', żeby łatwiej było rozpoznać symbole po samej wielkości, moim zdaniem obok siebie symbole @$ mogą się zlewać, podczas gdy przy symbolice @* widać od razu co jest graczem a co skrzynią
Nie dodałem pól gracza i skrzyni stojących na polu celu z dwóch powodów: po pierwsze zamiana tych symboli przy przejściu wygląda bardzo mylnie, na przykładzie z wiki:
#@$.  # -> # @*  # -> #  +$ # -> #  .@$#
W szczególności zmiany symboli @ -> + i $ -> *
Po drugie wymaga dodatkowych zmian w kodzie, a daje mało satysfakcjonujący efekt końcowy

2. Zmiany do kodu wprowadzałem tak, aby jak najlepiej odwzorować poprzednie działanie oraz przy okazji aby wyszło jak najmniej pracy
Tym samym każdy ekran z tekstem stał się samym tekstem wypisywanym w terminalu. W przypadku samej planszy zmianie podlegały tylko funkcje zwracające pojedyncze pole (teraz znaki) oraz całą planszę (wcześniej lista wszystkich obrazków z przesunięciem, teraz wygenerowane stringi pól w danym wierszu w kolejnych kolumnach będące reprezentacją wierszy w odpowiedniej kolejności i połączone znakami końca linii). W ten sposób wystarczyło zmienić tylko wynik każdej funkcji zwracającej Picture na Screen, który całkowicie zastąpił Picture i który w moim przypadku jest każdym możliwym stringiem (w szczególności w przypadku komunikatów nie jest to pole 23 x 79, a zwykły napis)

3. Implementacja runInteraction jest zbliżona do opisu z treści zadania, gdzie każde wywoływane go poza oczywistym inputem ma swój poprzedni stan. Obsługa strzałek jest najprostszą z możliwych (efekt braku dużej ilości czasu) - znaki '^' i '[' nie są znakami wpływającymi na rozgrywkę, więc są olewane, a znaki rozróżniające kody strzałek, czyli 'A', 'B', 'C' i 'D' są brane do handlera i on je rozpoznaje jako znaki strzałek. Poza tym możliwe jest poruszanie się poprzed wsad
Wyłączenie buforowania inputu nie miało u mnie żadnego wpływu, jednak zostało dołączone do rozwiązania, gdy okazało się, że jest to zależne tylko od ustawień lokalnych i w ogólności może być potrzebne
Plansza dość wyraźnie miga przy generowaniu kolejnych stanów planszy, co nie ma miejsca przy implementacjach gier zamieszczonych w internecie, ale nie mogłem zidentyfikować źródła problemu i tego, czy w ogóle jest on możliwy do naprawienia przeze mnie z poziomu kodu

4. Odnośnie zadanego pytania w ocenie z poprzedniego etapu: Poziomy nie są moje, znalazłem je gdzieś na Youtubie i ręcznie przepisałem
W szczególności również nie mogłem ich rozzwiązać ;)

W razie jakichkolwiek pytań proszę o kontakt mailowy

