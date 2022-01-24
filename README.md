# Latte compiler


## Biblioteki, pakiety i inne takie
Moje rozwiązanie wykorzystuje kilka Haskell'owych bibliotek.
- microlens-platform - mniejszy brat szkiełek. Biblioteki używam do lepszego operowania strukturami danych
- optparse-applicative - bardzo fajny parser linii argumentów
- mtl, filepath, text, containers, transformers  - kilka podstawowych bibliotek z monadami typu stejt, mapami, setami i innymi gadżetami

Rozwiązanie korzysta również z narzędzia `BNFC`, które generuje parser kodu źródłowego. 

## Budowanie rozwiązania
Pierwszym krokiem jest instalacja bibliotek opisanych w poprzednim punkcie. Prawdopodobnie wystarczy zainstalować:
```
cabal install happy
cabal install alex
cabal install microlens-platform
cabal install optparse-applicative
```
Kolejnym krokiem jest uruchomienie `make` w katalogu głównym

## Kilka informacji o rozwiązaniu
W katalogu głównym znajdują się
```
- app <- źródła pliku wykonywalnego latte
- src <- źródła biblioteki latte
  - IntermediateCode <-  definicje kodów pośrednich i konwertery
  - MachineCode <- generator kodu maszynowego
  - Optimizations <- funkcje optymalizujące kod czwórkowy
  - Parser <- funkcje parsujące kod źródłowy
    - BnfcParser <- kod wygenerowany przez BNFC
- test <- katalog z testami
  - IntegrationTests <- katalog z testami integracyjnymi
- lib <- katalog z dodatkowymi plikami wykonywalnymi
```

W ramach rozszerzeń zaimplementowałem:
- GCSE
- Struktury
- Tablice

Ponadto zaimplementowałem kilka innych optymalizacji:

- Dead code elimination
- Constant propagation
- Copy propagation
- Optymalizacja funkcji phi w kodzie czwórkowym

Kod maszynowy generuję mocno inspirując się kodem czwórkowym. Każda zmienna wynikowa z SSA ma swoje miejsce w pamięci. Każda operacja to tak naprawdę wczytanie wartości ze stosu do odpowiednich rejestrów, wykonanie operacji i zapisanie wartości na stosie.

### Robaki, o których wiem
Nie udało mi się poprawnie skonfigurować BNFC, efektem tego jest nie działający operator rzutowania. Musiałem go zastąpić przez sekwencję:
```
_( Type )_ Expr
_( list )_ null
```