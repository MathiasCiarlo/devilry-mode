#Devilry-mode

##Om
Devilry-mode gjør det enklere å rette obliger. Når du begynner å rette en oblig åpnes det automatisk en feedback-template, samt de to forrige tilbakemeldingene til studenten hvis disse finnes. Dette gjør at man enkelt kan se studentens utvilkling og dermed gi bedre tilbakemeldinger. Deretter vises README.txt hvis den finnes og alle javafiler kompileres, ved kompileringsfeil vises disse.
Foreløpig fungerer det kun for java-obliger (på grunn av kompileringen), med det kan enkelt endres.


##Installering
1) Last ned devilry-mode.el og legg den i en passende mappe, for eksemel "~/.emacs.d/plugins/devilry-mode/". Det enkleste er å clone repoet fra mappen "~/.emacs.d/plugins/"

2) Legg følgende linje i .emacsfilen din for å sørge for at devvilry-mode.el blir loadet når emacs starter.

``` elisp
;; Devilry-mode
(add-to-list 'load-path "~/.emacs.d/plugins/devilry-mode/")
(require 'devilry-mode)
```
Hvis du får feilmeldig ved oppstart, kontroller at "devilry-mode.el" ligger i riktig mappe.


##Bruk
- Skriv `M-x devilry-mode` (alt-x) for å aktivere modusen. Scriptet vil prøve å finne filen "devilry-mode.data" (data fra tidligere økter) og eventuelt opprette den hvis den ikke fins.
- Åpne obligfilene til den første studenten. Det mest effektive er å dra filene over i emacs med musa.
- Trykk f5 (eller skriv "M-x devilry-do-oblig")
- Scriptet finner brukernavnet på en syk måte, kontrollér at det er korrekt
- Rett obligen!
- f6 lukker alt du har åpent uten å lagre det, bruk dette før du starter på neste oblig


###Automatisk indentering
Det er inkludert funksjonalietet for å automatisk indentere all kode riktig når man begynner å rette en ny oblig, men dette anbefales ikke i inf1000, ettersom studentene trenger trening i å indentere riktig. For å legge til funksjonen: Endre `(setq devilry-indent-code nil)` til `(setq devilry-indent-code t)` øverst i "devilry-mode.el"

###Automatisk sletting av .classfiler
Endre `(setq devilry-rm-output-files nil)` til `(setq devilry-rm-output-files t)`

##Virkemåte
Devilry-mode bruker en feedbackmappe som inneholder én mappe for hver student. Hver gang man gir en tilbakemeldig, blir denne lagret i riktig mappe under navnet "<oblignummer>.txt".
Programmet bruker en fil "devilry-mode.data" for å lagre data fra forrige økt. Denne inneholder path til feedbackmappen, path til feedback-templaten og hviket oblignummer man er på. Hvis denne ikke finnes blir den opprettet ved oppstart av modusen.