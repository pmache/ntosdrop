# NTOSDrop — historical NT research reference

> **Status: Reference / Historical research tree / CODE REVIEWED**
>
> To repo nie jest aktywnym produktem portfolio. Jest zachowane jako materiał badawczy wokół Windows Research Kernel / Server 2003-era NT.

## Co to jest za projekt

`ntosdrop` jest historycznym drzewem NT/WRK z build scripts, starymi `.svn` metadata, prebuiltami i eksperymentalnymi zmianami społecznościowymi. Nie jest clean-room kernelem ani aktywną bazą do nowego systemu.

## Do czego służy

- analiza starego NT/WRK,
- porównania architektury kernela,
- badania nad starym toolchainem Microsoft,
- próby reprodukcji historycznych buildów w izolowanej VM,
- zachowanie zmian społecznościowych i eksperymentalnych patchy.

## Stan projektu

**Status: Reference / Historical / CODE REVIEWED**

Repo zawiera realny kod i historyczny build system, ale jego wartość jest badawcza. `Build.bat` mutuje tracked version files, generuje `ntverkp.h`, ma hard-coded `_Nt386Tree=B:\Bins.x86fre`, ustawia historyczne build-lab variables i zakłada obecność starego toolchainu/build.exe. To nie jest współczesny reproducible build path.

Nie należy wykonywać modernizacyjnych hotfixów bezpośrednio w historycznym drzewie, bo zatarłyby charakter snapshotu. Własne eksperymenty powinny powstawać jako osobne patchsety/clean-room projekty.

## Provenance i licencja

Repo należy traktować ostrożnie z punktu widzenia praw i pochodzenia plików.

Plik `LICENSE` opisuje się jako **BSD 2-Clause**, ale po standardowym tekście dodaje dodatkowe ograniczenie:

> `Any commercial usage is prohibited without conditions.`

Taki tekst **nie jest standardowym BSD-2-Clause**. Dodatkowo sama licencja repo nie rozstrzyga praw do kodu pochodzącego z WRK/NT ani innych źródeł. Dlatego:

- nie oznaczamy repo SPDX-em `BSD-2-Clause` bez wyjaśnienia,
- nie zakładamy prawa do komercyjnego reuse,
- nie kopiujemy kodu do aktywnych projektów bez per-file provenance/legal review,
- preferujemy clean-room reimplementation dla nowych projektów NT-like.

## Build feasibility

**Status: Historical / legacy toolchain required / NOT VERIFIED**

`Build.bat` zakłada m.in.:
- historyczne `WRKEnv.bat`, `build.exe`, `rc`, `lhBuildTag`,
- architekturę przekazywaną jako argument,
- konkretny układ katalogów `tools/<arch>`,
- hard-coded `B:\Bins.x86fre`,
- tracked pliki wersji modyfikowane podczas builda.

### Reference Gate NTD-R1 — Reproducible build

**PASS:** dokładna wersja toolchainu i środowiska jest odtworzona w izolowanej VM, a clean snapshot buduje się bez ręcznych zmian.  
**FAIL:** współczesny Windows/Visual Studio jest przedstawiany jako wystarczające środowisko bez dowodu.

### Reference Gate NTD-R2 — Provenance map

**PASS:** każdy fragment planowany do reuse ma znane źródło, autora i warunki licencyjne.  
**FAIL:** repo-level `LICENSE` jest traktowany jako automatyczna zgoda na reuse całego drzewa.

### Reference Gate NTD-R3 — Historical integrity

**PASS:** repo pozostaje historycznym snapshotem; eksperymentalne poprawki są utrzymywane osobno jako patchset/fork badawczy.  
**FAIL:** aktywne refaktory mieszają się z materiałem historycznym i utrudniają określenie provenance.

## Maintenance policy

Dozwolone:
- dokumentacja provenance,
- notatki reprodukcyjne,
- metadane wersji/toolchainu,
- katalog patchsetów, jeśli jest wyraźnie oddzielony od snapshotu.

Nie robimy z tego repo aktywnego kernela NT i nie przenosimy kodu do innych projektów bez osobnej analizy prawnej i architektonicznej.
