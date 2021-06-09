# XD

## Running

### Students machine
```
make
./interpreter prog.xd
```

### Machine with stack
```
stack build
stack run xd-lang prog.xd
```

### Features
Na 15 punktów:
* [x] 01 (trzy typy)
* [x] 02 (literały, arytmetyka, porównania)
* [x] 03 (zmienne, przypisanie)
* [x] 04 (print)
* [x] 05 (while, if)
* [x] 06 (funkcje lub procedury, rekurencja)
* [x] 07 (przez zmienną / przez wartość / in/out)

Na 20 punktów:
* [x] 09 (przesłanianie i statyczne wiązanie)
* [x] 10 (obsługa błędów wykonania)
* [x] 11 (funkcje zwracające wartość)

Na 30 punktów
* [x] 12 (4) (statyczne typowanie)
* [x] 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
* [x] 16 (1) (break, continue)

### Źródła
Kilka ogólnych rozwiązań jak np. struktura projektu czy zarządzanie stanem
zostało zaczerpniętych z następujących projektów:
* https://github.com/mbenke/toy-interpreters
* https://github.com/BAndysc/BInterpreter
* https://github.com/upicine/Capp-interpreter
* https://github.com/kkragoth-mimuw/lakke

### Język
Język składa się z listy definicji.
Uruchomienie zaczyna się do funkcji `main` która musi być typu int i nie posiadać parametrów.
Przed interpreterem uruchamiany jest `TypeChecker`.
Istnieją zmienne globalne, funkcje można dowolnie zagnieżdżać ze statycznym wiązaniem.
Możliwe niepoprawne użycie `break`, `continue` lub potencjalne wyjście z funkcji
złym typem sprawdzane jest na etapie `TypeCheckera`.
Dopuszczalne jest redaklarowanie zmiennej.

### Zmiany od rozmowy
* dodano typechecker
* dodano sensowną obsługę błędow
* rozwiązano konflikty w gramatyce
* usunięto krotki i tablice
* dodano przykłady bad (żeby je przetestować należy odkomentować poszczególne linijki)
* zmieniono `StmtEnding` aby uwzględniał możliwość zwrócenia nowego `Env`
* naprawiono problem z niezdefiniowanym przekazaniem przez referencje
* dodano operacje `+` dla stringów i printowanie intów
* naprawiono problem ze zmienną typu void
