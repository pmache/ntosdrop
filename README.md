# NTOSDrop — historical NT research reference

> **Status: Reference / Historical research tree**
>
> To repo nie jest aktywnym produktem portfolio. Jest zachowane jako materiał badawczy wokół Windows Research Kernel / Server 2003-era NT.

## Co to jest

Historyczny opis przedstawia `ntosdrop` jako zmodyfikowany kernel badawczy zgodny z Windows Server 2003 SP1, z eksperymentalnymi zmianami dotyczącymi m.in. optymalizacji, debugowania, monitoringu kernela i dodatkowych warstw kompatybilności.

## Provenance

Repo bazuje na istniejącym materiale WRK/NT i pracach społecznościowych. Nie należy przedstawiać go jako oryginalnego clean-room kernela ani przypisywać właścicielowi tego repo autorstwa bazowego kodu NT.

## Do czego służy to repo

- analiza starego NT/WRK,
- porównania architektury kernela,
- badania nad starym toolchainem Microsoft,
- reprodukcja historycznych buildów,
- zachowanie zmian społecznościowych i eksperymentalnych patchy.

## Build feasibility

**Status: Historical / legacy toolchain required**

Historyczny README wskazuje na build przez `build.cmd` i stare kompilatory Visual C++ 2003. Nie deklarujemy współczesnego wsparcia bez osobnej reprodukcji.

### Reference Gate NTD-R1 — Reproducible build

**PASS:** dokładna wersja źródeł, toolchainu i środowiska pozwala powtórzyć build w izolowanej VM.  
**FAIL:** README sugeruje, że wystarczy „uruchomić build” na dowolnym współczesnym Windows.

## Zakres historyczny

W źródłowej linii deklarowano m.in.:
- speed optimization,
- version customization,
- bug fixes,
- NT Kernel Monitor,
- enhanced debugging/startup,
- dodatkowe eksperymentalne warstwy.

Te elementy są materiałem do analizy, nie roadmapą aktywnego produktu.

## Legal / licensing note

Repo należy traktować ostrożnie z punktu widzenia provenance. Wszelkie użycie kodu zależy od faktycznego pochodzenia poszczególnych plików i ich licencji. Sam opis „GPLv3” nie powinien być interpretowany jako automatyczne rozwiązanie praw do kodu pochodzącego z innych źródeł.

## Maintenance policy

Dozwolone:
- dokumentacja provenance,
- notatki reprodukcyjne,
- metadane wersji/toolchainu,
- zachowanie historycznych patchy.

Nie rozwijamy tego jako nowego kernela NT i nie przenosimy kodu do aktywnych projektów bez osobnej analizy prawnej i architektonicznej.
