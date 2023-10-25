# Masterarbeit am Lehrstuhl für Digitale Transformation: Politische Medien --- mediale Politik? Eine explorativ-datengetriebene Analyse der Interaktionen politischer und medialer Akteure auf Twitter in Deutschland zwischen Januar 2022 und Oktober 2022

> Das Repository enthält Daten der Masterarbeit mit dem (Arbeits-)Titel *"Politische Medien --- mediale Politik? Eine explorativ-datengetriebene Analyse der Interaktionen politischer und medialer Akteure auf Twitter in Deutschland zwischen Januar 2022 und Oktober 2022"* am Lehrstuhl für Digitale Transformation der Otto-Friedrich-Universität Bamberg. Die Arbeit wird betreut durch *Prof. Dr. Andreas Jungherr* und wird vom Verfasser im Schwerpunkt *Computational Social Science* eingereicht.

## Thematischer Fokus
Die Arbeit beschäftigt sich mit den Wechselwirkungen in der Beziehung zwischen Medien bzw. dem medialen Akteursspektrum und Politik bzw. dem politischen Akteursspektrum in der Bundesrepublik Deutschland im Zeitraum von Januar 2022 bis Oktober 2022. Das Erkenntnisinteresse ist bewusst zumindest teils offen gelassen, da sich im Forschungsdesign bewusst für ein exploratives Auswertungsvorgehen entschieden worden ist, um eventuell in den Daten vorhandenen, interessanten Spuren nachgehen zu können.

Dabei wird die Datengrundlage durch ein breit angelegtes Webscraping der identifizierten Akteure auf der Plattform X (vormals: Twitter) gebildet. Dies bringt die Einschränkung mit sich, dass sich nur auf jene Akteure konzentriert werden kann, die auch auf dieser Plattform aktiv sind. Ein entscheidender Vorteil ist jedoch die Tatsache, dass die Daten somit alle "aus einer Hand" stammen, d. h. dass die Herkunft der Daten sowohl für die mediale als auch die politische Seite identisch ist, was das Erlangen dieser Daten vereinfacht.
Ebenso vorteilhaft ist der Stellenwert der Plattform selbst: bei X/Twitter handelt es sich heutzutage um eine wichtige Säule der digitalen Erweiterung von Öffentlichkeit bzw. des öffentlichen Raumes; in diesem Raum ist es Menschen möglich, sich auszutauschen und über ein breites Themenspektrum zu kommunizieren --- daraus resultiert, dass diese Plattform und ihre Infrastruktur prädestiniert für die sozial- und politikwissenschaftliche Forschung erscheinen.

## Zu adressierende Fragen
Fragen, die von vorliegender Arbeit adressiert werden, sind unter anderem die folgenden:

1. Welche Themen lassen sich in der medialen und politischen Landschaft auf Twitter in Deutschland über die Grenzen der jeweiligen Akteurssphären hinweg und über den beobachteten Zeitraum hinweg identifizieren?
2. Welche Muster lassen sich in der medialen Berichterstattung bzw. in der politischen Kommunikation zu diesen identifizierten Themen entdecken?
3. Welche Akteure innerhalb der jeweiligen Sphären dominieren Diskurse zu den identifizierten Themen?
4. Gibt es regionale Besonderheiten in der Adressierung von Themen in den beiden Akteurssphären? Wenn ja, welche möglichen Erklärungen kann es dafür geben?
5. In einem Vergleich der in 2. erwähnten Muster über die Grenzen der Akteurssphären hinweg: es kann unter Umständen angenommen werden, dass Themen von den beiden Akteurssphären auf unterschiedliche Art besprochen werden --- lassen sich diese Unterschiede in den Daten beobachten bzw. wiederfinden?

## Theoretischer Rahmen
Bezüglich des theoretischen Rahmens versucht die Arbeit, zu einem gewissen Teil Neuland zu beschreiten, indem eine theoretische Synthese dreier für das Spannungsverhältnis zwischen Medien und Politik zentraler Forschungsansätze angestrebt wird. Dabei handelt es sich um folgende Theorien:

1. Agenda Setting-Theorie
2. Indexing-Theorie
3. Mediatisierungsthese
    
Während beim *Agenda Setting* die richtungsgebende Funktion im Spannungsverhältnis von Seiten der Forschung bei den medialen Akteuren verortet wird, ist dies im Falle des sog. *Indexings* der umgekehrte Fall. Generell jedoch ist es nach Ansicht der vorliegenden Arbeit in einem Versuch der Weitung der theoretischen "Vogelperspektive" möglich und sinnvoll, diese beiden Theorien als Erscheinungsformen bzw. Interpretationen der Realität nach der *Mediatisierungsthese* anzusehen, die sich --- knapp zusammengefasst --- mit der immer weiter voranschreitenden und ausgreifenderen Durchdringung gesellschaftlichen Zusammenlebens durch das Mediale auseinandersetzt.

## Datengrundlage
Wie bereits angesprochen, wird dieses Forschungsvorhaben im Hinblick auf die Datengrundlage auf die durch ein eigens dafür vorgenommenes Webscraping der Plattform X/Twitter unterfüttert.

Dabei wurde sich für die politischen Akteure auf eine Liste aller Kandidierenden zur Bundestagswahl 2021 gestützt, welche durch das *Leibniz-Institut für Medienforschung | Hans-Bredow-Institut* gepflegt wird ([hier](https://github.com/Leibniz-HBI/DBoeS-data-BTW21) einzusehen). Dabei wurde nach jenen Kandidierenden gefiltert, die nach der fraglichen Bundestagswahl 2021 auch als gewählte MdBs in diesen eingezogen sind und darüber hinaus über einen Account bei X/Twitter verfügen.

Was die Repräsentation der Medienlandschaft Deutschlands angeht, so beschreitet die vorliegende Arbeit ebenso Neuland, da hier auf eine eigens erstellte Liste medialer Akteure zurückgegriffen wurde, welche in größtmöglicher Granularität (sortiert nach Bundesländern) sowohl regionale und lokale Printmedien beinhaltet als auch überregional (d. h. auf Bundesebene erscheinende) Printmedien umfasst. Weiterhin wurden bekannte bzw. vielgelesene, digitale Medien in dieser Liste abgebildet. Zuletzt beinhaltet sie außerdem die Medien des öffentlich-rechtlichen Rundfunks.
In einem weiteren Schritt wurde mit der Einordnung der Akteure auf der erstellten Liste nach den Merkmalen der *discursive power*-Theorie nach Jungherr, Posegga & An ([2019](https://journals.sagepub.com/doi/full/10.1177/1940161219841543)) ein weiterer Schritt hin zu einer differenzierten Auswertbarkeit der Daten getan. Auf diese Weise ist es möglich, Akteure in der Auswertung anhand von deren *Reichweite* bzw. *Erreichbarkeit*, deren *Normen und Werten* und deren *Geschäftsmodell* zu filtern bzw. zu sortieren und so eventuell unterschiedliche Arten des Berichtens über bestimmte Themen herauszuarbeiten.

Die auf diese Weise erreichte, hohe Granularität des Datensatzes bildet also ein Alleinstellungsmerkmal der vorliegenden Arbeit für den Raum der Bundesrepublik Deutschland, der zuvor noch nicht auf diese Weise abgebildet und erfasst worden ist.

## Methodische Herangehensweise

### Identifikation von Themen: Topic Models
Die angestrebten Ziele der Masterarbeit hängen eng damit zusammen, wie und ob es möglich ist, aus den diffusen Tweets der jeweiligen Akteure Themen abzuleiten und diese dann auszuwerten. Zu diesem Zweck müssen zunächst Themen im Gesamtkorpus der Tweets identifiziert werden.
Dies ist nach der Ansicht der vorliegenden Arbeit auf zwei Wegen möglich:

1. Das Aufstellen prominenter Themen durch Deduktion und durch eigene Erfahrungen aus dem fraglichen Zeitraum sowie folglich Zuordnung zu diesen Themen in einem zweiten Schritt.
 2. Die Identifikation von prominenten Themen durch algorithmische Rechenoperationen (*unsupervised learning*) und folgende Zuordnung zu diesen Themen in einem zweiten Schritt.

Aufgrund der Tatsache, dass es für die Bearbeitung der vorliegenden Masterarbeit lediglich einen Verfasser gibt, bietet sich eher die zweite Möglichkeit an, wofür sich schließlich auch entschieden wurde, da bei ersterer Möglichkeit die Gefahr bestünde, durch den eigenen Bias Themen zu übersehen --- einer Einzelperson ist weniger zuverlässig bzw. dazu im Stande, zentrale und wichtige Themen über den Zeitraum von zehn Monaten zweifelsfrei zu identifizieren, als dies eventuell in einem Team mit mehreren Personen möglich wäre, wobei man z. B. mit Mitteln wie Gruppendiskussionen oder gemeinsamem Brainstorming durchaus diesem Bias-Problem begegnen könnte.

Aus diesem Grund wurde sich für das *Topic Modeling* als Methode des *unsupervised learning* entschieden. Die Motivation dahinter ergibt sich ebenfalls aus zwei Punkten:

 1. Topic Models liefern Wörter, die in Bezug auf ein Thema oft zusammen genutzt werden. Dadurch ist eine höhere symbolische Repräsentation eines Themas nach dessen Identifikation durch den/die Forschende/n über die es repräsentierenden Wörter gewährleistet.
 2. Durch die strukturierende Wirkung der Topic Models auf den unstrukturiert vorliegenden Korpus kann für eine/n einzelne/n Forschende/n die Arbeit erleichtert werden, da eine notwendige Feedbackschleife mit Kollegen und Kolleginnen zumindest zum Teil ersetzt wird.

### Klassifizieren der Tweets: Bag of Words
Nachdem mittels Topic Models Themen erschlossen wurden, sollen diese den Tweets zugewiesen werden, um dann später die Datensätze ordnungsgemäß auswerten zu können. Hierzu bieten sich als Folgeoperation die *Bag of Words*-Modelle an: die bereits im Topic Modeling erhaltenen Wörter sollen dann --- nach Säuberung von mehrdeutigen Begriffen --- zusammen mit einigen qualitativ-deduktiven Ergänzungen als Wörterlisten dienen, die ein auf diese Weise gewonnenes Thema mit einer gewissen Genauigkeit beschreiben können.

Schließlich werden diese Wörterlisten *binär* genutzt: wenn in einem Tweet ein Wort eines Themas "Thema" auftaucht, so wird ihm im Datensatz algorithmisch der Wert 1 für die Variable "Thema" zugewiesen. Weist ein Tweet keines der Wörter eines Themas auf, so erhält er den Wert 0 für die Variable "Thema".

Bei der Klassifizierung stand ebenfalls im Raum, Methodiken des maschinellen Lernens zu nutzen (beispielsweise durch Training eines logistischen Classifiers auf Basis eines Subsets der Tweets). Jedoch werden für die sinnvolle Umsetzung und Implementierung eines solchen Klassifikationsmechanismus Grundlagendaten benötigt (nachfolgend: *ground truth*). Als ground truth werden Beobachtungen in einem Datensatz bezeichnet, für die bereits eine als wahr anzunehmende Zuordnung erfolgt ist. Dies geschieht im Bereich der Wissenschaft in den meisten Fällen innerhalb eines Forschungsteams mittels manuellem Codieren der Tweets durch die Forschenden. Alternativ ist auch die Klassifizierung zum Erwerb der ground truth-Daten durch manuelles Codieren von Experten auf einem Gebiet denkbar. Da für vorliegende Arbeit ein breites Themenspektrum zu erwarten steht und außerdem nicht mit hochkomplexen Themen zu rechnen ist, war letztere Möglichkeit hinfällig bzw. nicht anzuraten. Ebenso wurde sich aus dem Grund gegen die Klassifizierung mittels maschinellem Lernen entschieden, da es sich bei den im Scraping erworbenen Tweet-Daten um eine enorm große Datenmenge handelt. So ist es der gängige Standard, zwischen 20-30% der Gesamtdaten als ground truth zu nutzen, um einen Classifier zuverlässig zu trainieren. Um hier ein Praxisbeispiel zu geben: im Falle der Medien des Bundeslandes NRW konnten ~170.000 Tweets erfasst werden – dies hätte eine manuelle Codierung von 34.000 Tweets (für den Fall dass ground truth = 20% entspräche) erfordert; eine derartige Menge ist jedoch für eine Einzelperson nicht in vernünftigem Maße (v. a. zeitlich) zufriedenstellend zu bewerkstelligen. Daher wurde sich für den schon geschilderten Bag of Words-Ansatz entschieden.
