## FCVIQ

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0) [![Release](https://img.shields.io/gitea/v/release/juicinate/fcviq?gitea_url=https%3A%2F%2Fgit.disroot.org)](https://git.disroot.org/juicinate/fcviq) [![Gitea Last Commit](https://img.shields.io/gitea/last-commit/juicinate/fcviq?gitea_url=https%3A%2F%2Fgit.disroot.org)](https://git.disroot.org/juicinate/fcviq)

----

### Deutsch

Eine interaktive Version des Flämischen CVI Fragebogen.

#### Was es bietet

Die Applikaton soll es erleichtern, den Flämischen CVI Fragebogen auszufüllen. [Eine Live-Demo ist verfügbar.](https://juicinate.shinyapps.io/fcviq)

Der Fragebogen ermöglicht es Fachkräften, im Rahmen der Diagnostik gezielte Fragen an Eltern zu stellen, um mögliche Hinweise auf eine vorliegende cerebrale visuelle Wahrnehmungs- und Verarbeitungsstörung (CVI) zu erfassen und in Domänen einzuteilen. 

***Ein Fragebogen ersetzt keine medizinische Diagnose. Wenn Sie sich Sorgen machen, sprechen Sie mit Ihrer Kinderärtztin.***

#### Limitationen

Aktuell werden nur die 35 der 46 Fragen ausgewertet, die auch im Paper von Ben Itzhak et al. (2020) in der Faktoreananalyse eingesetzt wurden.

Im pdf-Report, den diese Applikation erzeugt sind die Zuordnungen der Fragen zu den jeweiligen Domänen gelistet

**Die eingetragenen Daten werden nicht gesichert. Nach 15 Minuten Inaktivität sind die Eingaben gelöscht. Ein PDF Report kann erzeugt werden.**

----

### English

An interactive version of the Flemish CVI Questionnaire.

#### What it offers

This application aims to make filling out the Flemish CVI Questionnaire easier. [A Live Demo is available.](https://juicinate.shinyapps.io/fcviq)

The questionnaire enables professionals to ask parents specific questions as part of the diagnostic process in order to identify possible indications of a cerebral visual impairment (CVI) and categorise them into domains. 

***A questionnaire does not substitute a medical diagnosis. If you are concerned, talk to your paediatrician ***. 

#### Limitations

Currently only the 35 questions from the five factor analysis in Ben Itzhak et al. (2020) are evaluated.

The pdf report notes which question belongs to which domain.

----

### Changelog

v1.4

- added waiting screen while rendering report
- cleanup and lint code

v1.3

- cleaned up js code and added shinyscroll
- added modal before resetting

v1.2

- added PDF report
- added optional patient data inputs for report
- added session reset button

### Links

[Disroot](https://git.disroot.org/juicinate/fcviq)

[Github](https://github.com/juicinate/fcviq)

[Live Demo](https://juicinate.shinyapps.io/fcviq)

### Quellen / References

Ortibus, E., Laenen, A., Verhoeven, J., De Cock, P., Casteels, I., Schoolmeesters, B., Buyck, A. and Lagae, L. (2011), Screening for Cerebral Visual Impairment: Value of a CVI Questionnaire. Neuropediatrics, 42(04): 138-147. [doi](https://doi.org/10.1055/s-0031-1285908)

Ben Itzhak, N., Vancleef, K., Franki, I., Laenen, A., Wagemans, J. and Ortibus, E. (2020), Visuoperceptual profiles of children using the Flemish cerebral visual impairment questionnaire. Dev Med Child Neurol, 62: 969-976. [doi](https://doi.org/10.1111/dmcn.14448)

Fragebogen modifiziert und übersetzt von J. Corazolla.

### Lizenz / License

&copy; 2025 J. Corazolla

Dieses Programm ist kostenlose Software. Es ist nach [AGPL-3.0](https://www.gnu.org/licenses/agpl-3.0.html) lizensiert. Dieses Programm wird in der Hoffnung verbreitet, dass es nützlich sein wird, jedoch OHNE JEGLICHE GARANTIE; sogar ohne die implizite Garantie der MARKTGÄNGIGKEIT oder EIGNUNG FÜR EINEN BESTIMMTEN ZWECK. 

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU Affero General Public License](https://www.gnu.org/licenses/agpl-3.0.html) for more details. 
