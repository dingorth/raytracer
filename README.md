# Ocaml Raytracer

## Kompilacja
    `make`

## Wywołanie
    - pokazanie obrazka `raytrace "json.json" "show"`
    - zapisanie obrazka `raytrace "json.json" "save" "path"`

## Opis generowanego obrazka

Opis obrazka jest w formacie json.
Składa się on ze sceny i kamery.

{
    "scene": {},
    "camera": {}
}

### Opis sceny

Scena zawiera dwa pola:
- listę świateł i listę obiektów.

Każdy obiekt światła ma swój typ, oraz odpowiednie dla niego parametry:
- "central" -> pozycja jako punkt w przestrzeni 3d, oraz jego moc w rgb
- "sunlight" -> wektor padania światła na obiekt, oraz moc światła w rgb

Każdy obiekt (robject) zawiera swój typ i odpowiednie dla niego parametry, 
którymi są "shape" i "surface". Dostępne "shape" to:
- "sphere" -> centrum sfery jako punkt w przestrzeni, jej promień
- "plane" -> parametry równania Ax + By + Cz + D = 0

Dostępne surface to:
- "scatter" -> zawiera kolor


### Opis kamery

Kamera zawiera pozycję jako 4 punkty w przestrzeni (odpowiedni A, B, C, D):
A  B
C  D,
Punkt w przestrzeni służący za ognisko. Musi znajdować się między obiektami na 
scenie a kamerą daną jw A, B, C, D.
Rozdzielczość obrazka w pixelach.


## Bardziej technicznie

### Moduł Vector3d
    Zawiera implementację wektorów w trójwymiarowej przestrzeni.

### Moduł Raytypes
    Zawiera typy podstawowych obiektów występujących w 
    programie takich jak abstrakcyjne: light, shape, surface,
    konkretne dziedziczące po abstrakcyjnych: robject, sunlight, central, scatter, plane, sphere,
    pozostałe: ray, color, pixel, camera, focus, picture, resolution ...


### Moduł Main
    Odpowiada za wczytanie danych od użytkownika, oraz odpowiednie 
    przekierowania wykonania programu do odpowiednich funkcji.

### Moduł Raymod
    Zawiera funkcje odpowiadające za parsowanie pliku json, oraz funkcję
    wykorzystujące abstrakcyjny interfejs oferowany przez klasy w module 
    Raytypes.

## Sprawozdanie

Wstępną trudnością okazało się dobre ustrukturyzowanie kodu i wybranie
odpowiednich do tego metod.
Wypadało również osiągnąć pewnego rodzaju polimorfizm, gdzie możemy
wywoływać funkcję liczące kolor promienia bez względu na typ obiektu, oraz
jego kształt. Również światła musiały posiadać tego rodzaju mechanizm.
Wtedy możemy używać jednej listy do obiektów tak na prawdę różnego typu.

Wstępnie przy kontroli postępów oddałem kod oparty na modułach. Udało mi się 
osiągnąć pewnego rodzaju polimorfizm, jednak to rozwiązanie nie było dla mnie 
wygodne.
Zdecydowałem się ostatecznie na klasy, ponieważ tak mi było łatwiej i schemat
metod wirtualnych jest mi dobrze znany.

Kolejnym problemem były cykliczne zależnośći między klasami.
Siłą rzeczy bardzo one od siebie zależą.
Rozwiązaniem było sparametryzowanie klas przez typy polimorficzne, za które
i tak podstawiałem tylko jeden "typ" typu.
