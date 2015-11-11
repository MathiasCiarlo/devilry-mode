#Devilry-mode

##Om
Devilry-mode gjør det enklere å rette obliger. Når du begynner å rette en oblig åpnes det automatisk en feedback-template, samt den forrige tilbakemeldingen til studenten hvis denne finnes. Dette gjør at man enkelt kan **se studentens utvilkling** og dermed gi **bedre tilbakemeldinger**. Deretter vises README.txt hvis den finnes og alle javafiler kompileres, eventuelle kompileringsfeil vises.
Pluginen ble laget for java-programmering, men kan brukes for i alle fag! Deaktiver i såfall javakompilering, se "Deaktiver javakompilering" nedenfor under "Innstillinger".


##Installering
- Clone repoet med git:
`git clone git@github.com:MathiasCiarlo/devilry-mode.git ~/.emacs.d/plugins/devilry-mode`

- Legg følgende linje i .emacs-filen din for å sørge for at devilry-mode.el blir lastet når emacs starter.

``` elisp
;; Devilry-mode
(add-to-list 'load-path "~/.emacs.d/plugins/devilry-mode/")
(require 'devilry-mode)
```
Hvis du får feilmeldig ved oppstart, kontroller at "devilry-mode.el" ligger i riktig mappe.


##Enkel bruk
- Kjør funksjonen devilry-mode `M-x devilry-mode` (alt-x) for å aktivere modusen. Scriptet vil prøve å finne filen "devilry-mode.settings" (data fra tidligere økter) og eventuelt opprette den hvis den ikke finnes.
- Første gangen du bruker devilry-mode må du skrive inn pathen til et feedback-directory. Her lagres alle tilbakemeldingene dine. Skriv inn pathen.
Eksempel: `"~/grl/inf1000/h15/feedback/"`.
- Skriv så inn path til en feedback-template, dette er en tekstfil som alle tilbakemeldingene vil bruke som mal.
- Åpne obligfilene til den første studenten. Det mest effektive er å dra filene over i emacs med musa. Du kan også bruke find-file (C-x C-f) og åpne en hel mappe på en gang (ved å avsluttte med stjerne).
- Trykk `f5` (eller `M-x devilry-do-oblig`)
- Rett obligen!
- `f6` lukker alt du har åpent uten å lagre det, bruk dette før du starter på neste oblig.
- Gjenta.

##Virkemåte
Devilry-mode bruker en feedbackmappe som inneholder én mappe for hver student. Hver gang man gir en tilbakemeldig, blir denne lagret i riktig mappe under navnet "<oblignummer>.txt".
Programmet bruker en fil "devilry-mode.settings" for å lagre data fra forrige økt. Denne inneholder path til feedbackmappen, path til feedback-templaten og hviket oblignummer man er på. Hvis denne ikke finnes blir den opprettet ved oppstart av modusen.

##Innstillinger
Alle innstillinger ligger i filen **devilry-mode.settings** som opprettes samme sted som du installerte devilry-mode første gang du starter Devilry-mode.

###Annet mappesystem enn standard devilry-nedlasting (Anbefales!)
[Gard Inge Rosvold](https://github.com/gardir) har laget et [pythonscript](https://github.com/gardir/Devilry_sort) som organiserer mappene lastet ned fra Devilry veldig fint, hvis du har organisert filene slik, må du sette sette variabelen `easy-file-system t`.

###Deaktivering av javakompilering
Sett variabelen `devilry-java-compilation nil`.

###Automatisk indentering
Det er inkludert funksjonalietet for å automatisk indentere all kode riktig når man begynner å rette en ny oblig, men dette anbefales ikke alltid, ettersom studentene trenger trening i å indentere riktig. For å legge til funksjonen: Sett variabelen `automatic-indentation t`.

###Automatisk sletting av .class-filer
Sett variabelen `rm-output-files t`
