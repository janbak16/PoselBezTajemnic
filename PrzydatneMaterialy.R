### https://github.com/mi2-warsaw/sejmRP

### W katalogu 'Dane' znajdują się dane w formie .rda pobrane za pomocą pakietu SejmRP. Dane te pchodzą z powyższego repozytorium.

# deputies
#   id_deputy        - id posła (001 - 518)
#   surname_name    - imię i nazwisko posła
load("Dane//deputies.rda")

# votings
#   id_voting       - id głosowania
#   nr_meeting      - numer spotkania
#   date_meeting    - data spotkania
#   nr_voting       - numer głosowania
#   topic_voting    - temat głosowania
#   link_results    - link do wyników głosowania
load("Dane//votings.rda")

# votes
#   id_vote         - id głosu posła
#   id_deputy       - id posła (001 - 518)
#   d_voting        - id głosowania
#   vote            - głos posła: "Za","Przeciw","Wstrzymał się","Nieobecny"
#   club            - przynaleźność posła
load("Dane//votes.rda")

# statements
#   id_statement    - id wypowiedzi
#   surname_name    - autor wypowiedzi
#   date_statement  - data wypowiedzi
#   statement       - treść wypowiedzi
load("Dane//statements.rda")


# TODO:

# 1. Doczytanie danych ze strony http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp : rozszerzenie informacji o posłach  # DONE
# 2. połączenie głosowania z treścią (m.in. poprzez http://www.sejm.gov.pl/Sejm7.nsf/terminarz.xsp)                 # DONE

##################################################################################################################

# 1.

require(rvest)
require(stringi)
require(dplyr)

# Funkcja pomocnicza
wybierzInfo <- function(x) {
    wybrany_indeks <- sum(stri_detect_fixed(informacje, x, case_insensitive=TRUE) * seq_along(informacje))
    if (wybrany_indeks > 0) {
        info <- stri_replace_first_fixed(informacje[wybrany_indeks], x, "")
        return(info)
    } else {
        return(NA)
    }
}

wyniki_pobierania_danych <- vector()

# Pętla po wszystkich posłach i posłankach
ProgressBar <- txtProgressBar(min = 0, max = 516, style = 3)
for (i in 1:516) {
    #TEST i <- 2
    id_posla <- deputies[i,1]
    sesja <- html_session(paste0("http://www.sejm.gov.pl/Sejm7.nsf/posel.xsp?id=",id_posla))
    informacje_poprawione <- rep(NA, 14)
    
    # PARTIA
    informacje <- html_node(sesja, ".partia") %>% html_node(".data") %>% html_children() %>% html_text()
    informacje <- repair_encoding(informacje)

    if (is.na(wybierzInfo("Wybrany dnia:"))) {
        informacje_poprawione[1] <- "K"
    } else {
        informacje_poprawione[1] <- "M"
    }
    
    if (informacje_poprawione[1] == "M") {
        informacje_poprawione[2] <- wybierzInfo("Wybrany dnia:")
        informacje_poprawione[3] <- wybierzInfo("Lista:")
        informacje_poprawione[4] <- wybierzInfo("Okręg wyborczy:")
        informacje_poprawione[5] <- wybierzInfo("Liczba głosów:")
        informacje_poprawione[6] <- wybierzInfo("Staż parlamentarny:")
        informacje_poprawione[7] <- wybierzInfo("Klub/koło:")
        informacje_poprawione[8] <- wybierzInfo("Wygaśnięcie mandatu:")
        informacje_poprawione[9] <- wybierzInfo("Ślubował")
    } else {
        informacje_poprawione[2] <- wybierzInfo("Wybrana dnia:")
        informacje_poprawione[3] <- wybierzInfo("Lista:")
        informacje_poprawione[4] <- wybierzInfo("Okręg wyborczy:")
        informacje_poprawione[5] <- wybierzInfo("Liczba głosów:")
        informacje_poprawione[6] <- wybierzInfo("Staż parlamentarny:")
        informacje_poprawione[7] <- wybierzInfo("Klub/koło:")
        informacje_poprawione[8] <- wybierzInfo("Wygaśnięcie mandatu:")
        informacje_poprawione[9] <- wybierzInfo("Ślubowała")
    }
    
    # CV
    informacje <- html_node(sesja, ".cv") %>% html_node(".data") %>% html_children() %>% html_text()
    informacje <- repair_encoding(informacje)
    
    informacje_poprawione[10] <- wybierzInfo("Data i miejsce urodzenia:")
    informacje_poprawione[11] <- wybierzInfo("Wykształcenie:")
    informacje_poprawione[12] <- wybierzInfo("Tytuł/stopień naukowy:")
    informacje_poprawione[13] <- wybierzInfo("Ukończona szkoła:")
    informacje_poprawione[14] <- wybierzInfo("Zawód:")
    
    # Podsumowanie
    setTxtProgressBar(ProgressBar, i)
    
    wyniki_pobierania_danych <- rbind(wyniki_pobierania_danych, informacje_poprawione)
    
    Sys.sleep(3) ### Opóźnienie z uwagi na możliwość blokady?
}

deputies_ext_temp <- cbind(deputies, wyniki_pobierania_danych)
colnames(deputies_ext_temp) <- c("id_posel", "nazwisko_imie", "plec",
                                 "wybory_data", "wybory_lista", "wybory_okreg", "wybory_liczba_glosow",
                                 "staz_parlamentarny", "klub_kolo", "wygasniecie_mandatu", "slubowanie_data",
                                 "data_miejsce_urodzenia", "wyksztalcenie", "tytul_stopien_naukowy", "ukonczona_szkola", "zawod")

save(deputies_ext_temp, file = "dane//deputies_ext_temp.rda") # tymczasowo, zeby juz tego drugi raz nie pobierac






# wybory_okreg rozbite na wybory_okreg_id i wybory_okreg_miejscowosc
deputies_ext_temp <- mutate(deputies_ext_temp, wybory_okreg_miejscowosc = stri_trim(stri_match_first_regex(deputies_ext_temp$wybory_okreg, "\\s(.*)")[,1]),
                                               wybory_okreg_id = stri_match_first_regex(deputies_ext_temp$wybory_okreg, "\\w{1,2}"))

# staz_parlamentarny rozbity na czy_posel_..._kadencji oraz czy_senator_..._kadencji

deputies_ext_temp <- mutate(deputies_ext_temp, czy_posel_IX_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł IX kadencji"),
                                               czy_posel_X_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł X kadencji"),
                                               czy_posel_I_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł I kadencji"),
                                               czy_posel_II_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł II kadencji"),
                                               czy_posel_III_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł III kadencji"),
                                               czy_posel_IV_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł IV kadencji"),
                                               czy_posel_V_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł V kadencji"),
                                               czy_posel_VI_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "poseł VI kadencji"),
                                               czy_senator_I_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "senator I kadencji"),
                                               czy_senator_II_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "senator II kadencji"),
                                               czy_senator_III_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "senator III kadencji"),
                                               czy_senator_IV_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "senator IV kadencji"),
                                               czy_senator_V_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "senator V kadencji"),
                                               czy_senator_VI_kadencji =  stri_detect_fixed(deputies_ext_temp$staz_parlamentarny, "senator VI kadencji"))


# wygasniecie_mandatu rozbite na wygasniecie_mandatu_data oraz wygasnienie_mandatu_powod
deputies_ext_temp <- mutate(deputies_ext_temp, wygasniecie_mandatu_data = stri_sub(deputies_ext_temp$wygasniecie_mandatu, 1, 10), 
                                               wygasniecie_mandatu_powod = stri_sub(deputies_ext_temp$wygasniecie_mandatu, 11))

# data_miejsce_urodzenia rozbite na data_urodzenia oraz miejsce_urodzenia
deputies_ext_temp <- mutate(deputies_ext_temp, data_urodzenia = stri_sub(deputies_ext_temp$data_miejsce_urodzenia, 1, 10), 
                                               miejsce_urodzenia = stri_sub(deputies_ext_temp$data_miejsce_urodzenia, 13))

# nie uwzgledniac: wybory_data
# nie uwzgledniac: wybory_okreg
# nie uwzgledniac: staz_parlamentarny
# nie uwzgledniac: klub_kolo
# nie uwzgledniac: wygasniecie_mandatu
# nie uwzgledniac: data_miejsce_urodzenia

deputies_extension <- select(deputies_ext_temp, id_posel,
                                                nazwisko_imie,
                                                plec,
                                                wybory_lista,
                                                wybory_okreg_id,
                                                wybory_okreg_miejscowosc,
                                                wybory_liczba_glosow,
                                                czy_posel_IX_kadencji,
                                                czy_posel_X_kadencji,
                                                czy_posel_I_kadencji,
                                                czy_posel_II_kadencji,
                                                czy_posel_III_kadencji,
                                                czy_posel_IV_kadencji,
                                                czy_posel_V_kadencji,
                                                czy_posel_VI_kadencji,
                                                czy_senator_I_kadencji,
                                                czy_senator_II_kadencji,
                                                czy_senator_III_kadencji,
                                                czy_senator_IV_kadencji,
                                                czy_senator_V_kadencji,
                                                czy_senator_VI_kadencji,
                                                wygasniecie_mandatu_data,
                                                wygasniecie_mandatu_powod,
                                                slubowanie_data,
                                                data_urodzenia,
                                                miejsce_urodzenia,
                                                wyksztalcenie,
                                                tytul_stopien_naukowy,
                                                ukonczona_szkola,
                                                zawod)

save(deputies_extension, file = "dane//deputies_extension.rda")

##################################################################################################################

# 2.

require(rvest)
require(stringi)
require(dplyr)

votings_ext_temp <- mutate(votings, godzina = NA, opis1 = NA, opis2 = NA)

# Pętla po wszystkich głosowaniach
ProgressBar <- txtProgressBar(min = 0, max = 6212, style = 3)
for (i in 1:6212) {
    #TEST i <- 3432
    sesja <- html_session(votings$link_results[i])
    
    data_pelna <- html_node(sesja, "small") %>% html_text()
    godzina <- stri_trim(stri_match_first_regex(data_pelna, "godz\\.(.*)"))[2]
    
    opis1 <- html_node(sesja, "big") %>% html_text()
    opis1 <- stri_encode(opis1, from = "UTF-8")

    if (i == 3432) {
        opis2 <- NA
    } else {
        opis2 <- html_node(sesja, "font") %>% html_text()
        opis2 <- stri_encode(opis2, from = "UTF-8")
    }
    
    votings_ext_temp$godzina[i] <- godzina
    votings_ext_temp$opis1[i] <- opis1
    votings_ext_temp$opis2[i] <- opis2
    
    # Podsumowanie
    setTxtProgressBar(ProgressBar, i)

    Sys.sleep(3) ### Opóźnienie z uwagi na możliwość blokady?
}

colnames(votings_ext_temp) <- c("id_glosowanie", "posiedzenie_nr", "posiedzenie_data", "glosowanie_nr", "glosowanie_temat_stare", 
                                "link_do_wynikow", "glosowanie_godzina", "glosowanie_temat_ref", "glosowanie_temat")

save(votings_temp, file = "dane//votings_ext_temp.rda") # tymczasowo, zeby juz tego drugi raz nie pobierac



votings_extension <- select(votings_ext_temp, id_glosowanie,
                            posiedzenie_nr,
                            posiedzenie_data,
                            glosowanie_godzina,
                            posiedzenie_data,
                            glosowanie_temat_ref,
                            glosowanie_temat,
                            link_do_wynikow)

save(votings_extension, file = "dane//votings_extension.rda")

##################################################################################################################

# 3.

require(rvest)
require(stringi)
require(dplyr)


#wypowiedzi, oswiadczenia, interpelacje itd.

# Pętla po wszystkich posłach i posłankach
ProgressBar <- txtProgressBar(min = 0, max = 516, style = 3)
for (i in 1:516) {
    #TEST i <- 2
    id_posla <- deputies[i,1]
    sesja <- html_session(paste0("http://www.sejm.gov.pl/Sejm7.nsf/wypowiedzi.xsp?id=",id_posla,"symbol=WYPOWIEDZI_POSLA"))

    
}
