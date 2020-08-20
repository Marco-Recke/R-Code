pacman::p_load(tidyverse)
n_elements <- 5*10^3
menage <- tibble(salzstreuer=paste('Salz',1:n_elements),
                 pfefferstreuer=paste('Pfeffer',1:n_elements))
str(menage)

#Adressiert Salzstreuer, Salz und 50 Pfefferkörner

menage['salzstreuer']
menage[1]
menage %>% select(salzstreuer) 
select(.data = menage,salzstreuer)
menage[['salzstreuer']]
menage$salzstreuer
menage[[1]][1:100]
menage %>% slice(1:100) %>% pull(salzstreuer)

menage %>% slice(1:50) %>% pull(pfefferstreuer)

menage <- tibble(salzstreuer=rep('salt crystal',times=n_elements),
                 pfefferstreuer=rep('pepper corn',times=n_elements))

# slice Auswahl einer oder mehrer nzelne Zeilen /auswählen (alle spalten)
# pull gibt den Inhalt einer Spalte (Vektor)
# select: Auswahl einer oder mehrer Spalten/ die Spalte als Spaltenstruktur (tibble)
# [[]] pull
# [] select