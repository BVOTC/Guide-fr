###### Black Voices on the City Shiny App -FRENCH VERSION ########




# Load Libraries
library(shiny)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinydashboard)

Sys.setlocale(locale = "French")

Sys.setenv(LANG = "fr")




######### Import guide data ################################

## Load data
data <- read_csv("data/Guide_Sources.csv") %>%
  
  select(1:15)


#make sure there are no duplicate titles
duplicates <- data %>% filter (duplicated(Title)) %>% select(Title)
# 
# #if there are duplicated titles, make sure they are different languages - THIS SHOULD HAVE 0 ROWS
data %>% filter(Title %in% duplicates) %>% filter(duplicated(Language))


data <-  data %>% mutate(Publication  = if_else(is.na(Publication), Publisher, Publication) ) %>% select(-Publisher)

## Convert to french

data <- data %>% set_names( c("Auteur", "Auteurs additionnels", "Titre", "Année", "Revue / Éditeur", "Format", 
                              
                              "Link", "Region", "Pays", "Langue", "Theme_1", "Theme_2", "Theme_3", "Theme_4")) %>% 
  mutate(Langue = case_when(Langue == "English" ~ "Anglais",
                            Langue == "French" ~ "Français"  ),
         Format = case_when(Format == "Book - Entire" ~ "Livre - Entier",
                            Format == "Book - Chapter" ~ "Chapitre du livre
",
                            Format == "Journal Article" ~ "Article académique",
                            Format == "Online Material" ~ "Ressource en ligne",
                            Format == "Report" ~ "Rapport",
                            Format == "Thesis" ~ "Thèse",
                            Format == "Video" ~ "Vidéo",
                            Format == "News Article" ~ "Article d'actualité",
                            Format == "Podcast" ~ "Balado",
                            Format == "Multimedia / Other" ~ "Multimédia / Autre" ,
                            TRUE ~ "TKTK"),
         Region = case_when(Region == "North America" ~ "Amérique du Nord",
                            Region == "Caribbean" ~ "Caraïbes",
                            Region == "Africa" ~ "Afrique" ,
                            TRUE ~ Region),
         Pays = case_when(Pays == "United States" ~ "États Unis", 
                          Pays == "Belgium" ~ "Belgique", 
                          Pays == "England" ~ "Angleterre", 
                          Pays == "Ivory Coast" ~ "Côte d'Ivoire", 
                          Pays == "Multiple countries within one region" ~ "Plusieurs pays dans une région",
                          Pays == "Multiple countries / Global" ~ "Dans plusieurs pays / global",
                          TRUE ~ Pays)     ) %>% 
  mutate(across(11:14, ~case_when( 
    . == "Architecture and Urban Design" ~ "Architecture et design urbain",
    . == "Black Perspectives on Planning Practice and Education" ~ "Perspectives noires sur la pratique et l’éducation en urbanisme",
    . == "Community Organizing and Citizen Participation" ~ "Organisation communautaire et participation publique",
    . == "Crime, Policing, and Surveillance" ~ "Crime, police et surveillance",
    . == "Culture, Placemaking, and Black Geographies" ~ "Culture, “placemaking” et géographies noires",
    . == "Development and Gentrification" ~ "Développement et embourgeoisement",
    . == "Feminist and Queer Urbanism" ~ "Urbanisme féministe et queer",
    . == "Mapping and GIS" ~ "Cartographie et SIG",
    . == "Municipal Policy and Governance" ~ "Politique municipale et gouvernance",
    . == "Politics of Land, Property, and Colonialism" ~ "Politiques territoriales, propriété et colonialisme",
    . == "Public Housing and Cooperatives" ~ "Logement social et coopératives",
    . == "Public Space and Parks" ~ "Espaces publics et parcs",
    . == "Racial and Social Justice" ~ "Race et justice sociale",
    . == "Segregation and Redlining" ~ "Ségrégation et “redlining”", 
    . == "Sustainability, Environment, and Health" ~ "Durabilité, environnement et santé",
    . == "Transportation" ~ "Transport et mobilité",
    . == "Urban History" ~ "Histoire urbaine",
    is.na(.) ~ NA_character_,
    TRUE ~ "TKTK"   )))

### DATA VALIDATION - Check to make sure there are no incorrect entries for theme -- THIS SHOULD BE 0 ROWS
invalid <- data %>% filter(Format == "TKTK" | Theme_1 == "TKTK" | Theme_2 == "TKTK" | Theme_3 == "TKTK" | Theme_4 == "TKTK")

# Pivot Longer
data <- data %>% pivot_longer(cols = 11:14,
                              names_to = "Theme Type",
                              values_to = "Themes") %>%
  select(-11) %>%
  
  # Pivot Wider
  pivot_wider(names_from = Themes,
              values_from = Themes) %>%
  select(-14) %>%
  
  mutate(
    
    # create column just to make an "All" button easier
    Tous = "Tous",
    
    
    # Create new Item Format column (remove book redundancy)
    
    item_format_2 = `Format` %>%
      replace(`Format` == "Livre - Entier" |
                `Format` == "Chapitre du livre", 
              "Livre"),
    
    ##Embed hyperlinks in the item titles
    Titre = if_else(is.na(Link),
                    Titre,
                    paste0("<a href='",Link,"' target='_blank'>", Titre,"</a>")) 
    
    
    
  ) %>%
  
  select(1:4, Pays,  `Revue / Éditeur`, Format, everything())







#save(data, file = "data/data.Rdata")

###### Import additional resources data ###############################

data_AR <- read_csv("data/More_Resources.csv") %>%
  mutate( Name = if_else(is.na(Link),
                         Name,
                         paste0("<a href='",Link,"' target='_blank'>", Name,"</a>")),
          Language = case_when(Language == "English" ~ "Anglais",
                               Language == "French" ~ "Français"),
          Type = case_when(Type == "Resource List" ~ "Liste des ressources",
                           Type == "Podcast" ~ "Balado",
                           Type ==  "People to Follow" ~ "Personne à suivre",
                           TRUE ~ Type,
          ),
          Location = case_when(Location == "North America" ~ "Amérique du Nord",
                               Location == "Caribbean" ~ "Caraïbes",
                               Location == "Africa" ~ "Afrique" ,
                               Location == "United States" ~ "États Unis", 
                               Location == "Belgium" ~ "Belgique", 
                               Location == "England" ~ "Angleterre", 
                               Location == "Ivory Coast" ~ "Côte d'Ivoire", 
                               TRUE ~ Location)) %>%
  select( Langue = Language, Nombre = Name, Emplacement = Location, Type, Notes = Description) 



###### Webpage layout ################################################

#load(file = "data/data.Rdata")



tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                            
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: balance;
                                   -column-fill: balance;
                                     -webkit-column-gap: 0.7em;
     -moz-column-gap: 0.7em;
          column-gap: 0.7em
                                 } 
                                 "))  ))

### UI ###

ui <- fluidPage(tweaks,
                includeCSS("www/bootstrap.css"),
                
                ## adds logo in browser tab
                list(tags$head(HTML("<link rel='icon' type='image/gif/png' href='Logo-Dark-Fresno-6.png'>"))),
                
                navbarPage(
                  windowTitle	= "BVOTC Guide",          ##adds title in browser tab
                  
                  ##hack to get English/French option in the tabPanel
                  title = tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"display: block; float:right;padding: 14px 16px;   color: #333;text-decoration: none; font-family: Barlow; font-style: italic; font-weight: 400; font-size: 15px; line-height: 18px;\"> <a href=\"https://bvotc.shinyapps.io/Guide/\"> EN </a> / FR </div>');"  )),
                  
                  
                  tabPanel("Le guide",
                           
                           fluidRow(
                             column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                                    img(src='MainPageBanner.png', width = '102.2%'))
                           ),
                           
                           br(), br(),
                           fluidRow(
                             column(10, offset = 1,
                                    titlePanel(h1("Parcourir le guide de ressources")))),
                           fluidRow(
                             column(10, offset = 1,
                                    helpText(p(style="text-align: justify;", 
                                               h4("Voix noires sur la ville est une base de données organisée par des étudiantes et étudiants qui vise à accomplir deux choses: premièrement, répertorier les contributions de chercheuses et chercheurs, ainsi que des professionnelles et professionnels noirs au domaine de l'urbanisme, et deuxièmement, amplifier les voix et les perspectives uniques qu'elles et ils apportent à une discipline qui a été largement dominée par les hommes blancs, cisgenres et hétérosexuel depuis sa création. En tant qu’étudiantes et étudiants ainsi que des graduées et gradués de programmes d'urbanisme canadiens, l'élaboration de ce guide est née de nos frustrations face au manque d’une diversité de perspectives dans nos salles de classe et nos programmes, un manque trop souvent non reconnu. Ce guide n'est qu'une partie d'un effort plus vaste et naissant visant à organiser le corps étudiant et professoral autour de la réinvention de ce que constitue l'urbanisme et de qui contribue à ce dialogue."))),
                                    helpText(p(style="text-align: justify;", 
                                               h4("Pour ajouter une documentation au guide BVOTC, veuillez utiliser notre", 
                                                  tags$a(href = "https://forms.gle/FMakjhkWRWna5yf29", "formulaire"),
                                                  "ou envoyez-nous un courriel à",  tags$a(href = "mailto: bvotc.guide@gmail.com", "bvotc.guide@gmail.com"))))
                             )),
                           
                           fluidRow(
                             column(10, offset = 1,
                                    wellPanel(
                                      h3("Thèmes"),
                                      h5(  tags$div(align = 'left', 
                                                    class = 'multicol', 
                                                    radioButtons(inputId = "keyword",
                                                                 
                                                                 label = NULL,
                                                                 choices = c("Tous", "Architecture et design urbain", 
                                                                             "Cartographie et SIG", 
                                                                             "Crime, police et surveillance",
                                                                             "Culture, “placemaking” et géographies noires",
                                                                             "Développement et embourgeoisement", 
                                                                             "Durabilité, environnement et santé", 
                                                                             "Espaces publics et parcs", 
                                                                             "Histoire urbaine", 
                                                                             "Logement social et coopératives" ,
                                                                             "Organisation communautaire et participation publique", 
                                                                             "Perspectives noires sur la pratique et l’éducation en urbanisme",
                                                                             "Politique municipale et gouvernance",
                                                                             "Politiques territoriales, propriété et colonialisme",
                                                                             "Race et justice sociale", 
                                                                             "Ségrégation et “redlining”",
                                                                             "Transport et mobilité",
                                                                             "Urbanisme féministe et queer"),
                                                                 
                                                                 inline = F))),
                                      
                                      
                                      h5(checkboxGroupInput(inputId = "type",
                                                            label = h3("Type de ressources"),
                                                            choices = c("Livre",  "Article académique",
                                                                        "Rapport", "Thèse",
                                                                        "Article d'actualité", "Ressource en ligne", 
                                                                        "Balado", "Vidéo",
                                                                        "Multimédia / Autre" ),
                                                            selected = c("Livre",  "Article académique",
                                                                         "Rapport", "Thèse",
                                                                         "Article d'actualité", "Ressource en ligne", 
                                                                         "Balado", "Vidéo",
                                                                         "Multimédia / Autre" ),
                                                            inline = T)),
                                      
                                      h5( chooseSliderSkin(
                                        skin = "Flat",
                                        color = "#3A469D"
                                      ),
                                      sliderInput (inputId = "years",
                                                   label = h3("Année de publication"),
                                                   1890, 2020, 
                                                   value = c(1890, 2020),
                                                   sep = "")),
                                      
                                      
                                      h5(checkboxGroupInput(inputId = "location",
                                                            label = h3("Emplacement"),
                                                            choices = c("Tous",
                                                                        "Amérique du Nord",
                                                                        "Caraïbes",
                                                                        "Afrique", "Europe", "Global"
                                                                        
                                                            ),
                                                            selected = "Tous",
                                                            inline = T)),
                                      h5(checkboxGroupInput(inputId = "language",
                                                            label = h3("Langue"),
                                                            choices = c("Français", "Anglais"),
                                                            selected = "Français", inline = T))
                                      
                                      # h5(  selectInput(inputId = "language",
                                      #                  label = h3("Langue"),
                                      #               choices = c("Tous", "Anglais", "Français"),
                                      #                  selected = "Français",
                                      #                  width = "25%"))   
                                    ))
                           ),
                           br(),
                           fluidRow(
                             column(10,offset = 1,
                                    h5( dataTableOutput("table")))
                           )
                           
                  ),
                  
                  tabPanel("Mode d'emploi",
                           br(), br(),
                           tags$a(href='https://bvotc.shinyapps.io/Guide-fr/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                           
                           fluidRow(
                             column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                                    img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                           ),
                           br(), br(),
                           
                           
                           fluidRow(
                             column(10, offset = 1,
                                    helpText(p(style="text-align: justify;",
                                               h4("Nous espérons que ce guide se révélera d'une grande utilité pour les membres du grand public intéressés par les questions d'urbanisme et de politique, et nous espérons surtout qu'il provoquera une réflexion plus approfondie au sein des programmes universitaires d’urbanisme pour changer les méthodes d'enseignement et d'apprentissage afin d’inclure les voix de personnes noires et les approches auparavant sous-représentées sur l'environnement bâti."))),
                                    br(),
                                    helpText(h3("Pour les membres du corps professoral,")),
                                    helpText(p(style="text-align: justify;",
                                               h4("Cela équivaut à s'assurer que les programmes et les discussions en classe entrent en dialogue avec les écrits et la pratique des urbanistes noir.e.s, à la fois historiques et contemporains. Bien que les programmes actuels puissent reconnaître un racisme historique en urbanisme, tel que celui associé aux processus de revitalisation urbaine, les diplômées et diplômés des programmes d'urbanisme doivent être exposés à la richesse des travaux récents de personnes noires qui abordent les formes de racisme rencontrées dans le monde professionnel. Plus essentiel encore, nous incluons des ressources dans tous les domaines de l’urbanisme. L’ensemble de ces documents ne se concentrent pas uniquement sur les questions raciales, car il est essentiel de ne pas limiter l'autorité de ces écrivaines et écrivains aux questions liées à la racialité. Ce sont des expertes et des experts ainsi que des leaders dans leurs domaines respectifs, y compris les transports, la durabilité et la gouvernance urbaine, et devraient être traités comme tels dans les cours de planification de premier cycle et des cycles supérieurs. Si vous ne vous sentez pas à l'aise de présenter ce matériel, surtout si vous êtes une personne blanche, alors c'est une occasion d'apprendre et de grandir avec vos étudiantes et étudiants, pas une excuse pour vous désengager.
"))),
                                    br(),
                                    helpText(h3("Pour les membres du corps étudiant,")),
                                    helpText(p(style="text-align: justify;",
                                               h4("Ce guide est un appel à l'action. Vous devriez l'utiliser pour vous organiser avec vos pairs et insister pour que des perspectives plus diverses de la discipline soient incluses dans votre éducation - pas seulement les voix de personnes noires, mais aussi celles des femmes, des universitaires issu.e.s des minorités visibles, des personnes LGBTQ, des personnes à mobilité réduite, de même que les communautés autochtones et immigrantes. Vous devez également l'utiliser comme une ressource pour l'apprentissage personnel et comme matériel de référence pour vos cours. Chaque essai, mémoire ou thèse est l'occasion de lire, d'apprendre et de citer des sources de personnes noires. C'est aussi l'occasion de repousser les limites de ce qui constitue une source « appropriée » pour le travail académique. Vous y trouverez des vidéos, des baladodiffusions, des blogs, des interviews et d'autres formes de médias qui représentent un plus large éventail de la pensée noire sur les questions urbaines que celles que celles produites par les revues universitaires traditionnelles et les maisons d'édition. Enfin, ce guide se veut un point de départ et nous vous encourageons à le soutenir en y contribuant au fur et à mesure que de nouvelles ressources seront disponibles.
"))),
                                    br(),
                                    helpText(h3("Pour le grand public,")),
                                    helpText(p(style="text-align: justify;",
                                               h4("Ce guide offre quantité de lectures pour remettre en question les hypothèses sur la race, l’espace et la construction du sens dans l'environnement urbain. Il s'étend de l'histoire de la ségrégation résidentielle et des disparités dans les transports publics à la cartographie de la joie noire [black joy] et aux réalisations des architectes noir.e.s. Bien que de nombreux articles de revues et ouvrages scientifiques ne soient accessibles que par un accès institutionnel (c.-à-d. universitaire), vous pouvez vérifier si votre bibliothèque publique locale offre un accès en ligne ou par le biais de programmes de Prêt entre bibliothèques (PEB). De nombreux livres sont disponibles dans les librairies locales.
"))),
                                    br(),
                                    
                                    helpText(h3("Accéder aux ressources")),
                                    helpText(p(style="text-align: justify;",
                                               h4("Même si nous aspirons à fournir l’accès aux ressources gratuitement, nous reconnaissons qu’une partie de la documentation n’est pas accessibles pour celles et ceux ayant des moyens financiers limités ou celles et ceux sans accès au réseau des bibliothèques universitaires. Nous croyons que le manque d’argent ne devrait pas être un frein à la connaissance et encourageons les utilisateurs.trices à explorer les alternatives suivantes pour accéder aux ressources.
"),
                                               br(),
                                               h4("Pour les livres:"),
                                               h5("- Explore le catalogue de ta bibliothèque locale or look at Interlibrary Loan (ILL) programs"),
                                               h5("- Explore ta librairie locale (", tags$a(href = "https://www.indiebound.org/indie-bookstore-finder", "États-Unis"),",", tags$a(href = "https://www.google.com/maps/d/u/0/viewer?mid=19gEK_fkWpBbp0Hvba32T5T77YzjosqXI&ll=43.30929915907397%2C-80.25552581529155&z=9", "Canada") ,")"),
                                               h5("- Effectue une recherche sur un moteur de recherche avec le titre et « free PDF »
"),
                                               h5("- Visite", tags$a(href = "https://emilkirkegaard.dk/en/?p=7172", "libgen"),  "(fais",
                                                  tags$a(href = "https://medium.com/@sqmblog/is-library-genesis-libgen-safe-a40c8d33b0b9", "attention"), 
                                                  "aux téléchargement à partir de pages web) "),
                                               br(),
                                               h4("Pour les articles academiqies:"),
                                               h5("- Vérifie si ta bibliothèque locale a accès à une base de donnée"),
                                               h5("- Examine toutes les versions sur Google Scholar (cherche le PDF dans la marge de droite)"),
                                               h5("- Contacte l’auteur/les auteurs directement pour demander l’accès (Généralement, ils ne reçoivent pas de part donc il seront heureux de partager leur travail!)
"),
                                               h5("- Consulte", tags$a(href = "https://www.facebook.com/groups/850609558335839", "les groupes de partage de PDF"),
                                                
                                                "sur Facebook"),
                                               h5("- Visite",tags$a(href = "https://emilkirkegaard.dk/en/?p=7165", "Sci-hub")  )
                                               
                                    )),
                                    
                                    
                                    br())),
                           br(),
                           
                           fluidRow(
                             column(10, offset = 1,   
                                    helpText(p(style="text-align: justify;",
                                               h4("Merci de signaler tout hyperlien rompu ou toutes corrections à", tags$a(href = "mailto: bvotc.guide@gmail.com", "bvotc.guide@gmail.com"))) )),
                             br(),
                             br(),
                             br() )
                           
                  ),
                  
                  tabPanel("À propos",
                           br(), br(),
                           tags$a(href='https://bvotc.shinyapps.io/Guide-fr/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                           fluidRow(
                             column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                                    img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                           ),
                           br(), br(),
                           fluidRow(
                             column(10, offset = 1,
                                    helpText(h3("Qui sommes-nous")),
                                    helpText(p(style="text-align: justify;",
                                               h4("L'une des questions les plus importantes dans les enseignements antiracistes est: « Qui suis-je / qui sommes-nous pour faire ce travail? ». Nous devons aux utilisatrices et utilisateurs de ce guide une réponse à cette question afin qu'elles et eux puissent nous demander de rendre des comptes alors que nous utilisons nos privilèges pour lutter contre le racisme anti-noir dans le domaine de l'urbanisme. Tout d'abord, nous sommes un groupe d'étudiantes et d’étudiants aux cycles supérieurs, ainsi que de graduées et de gradués majoritairement non-noirs dont la vie a bénéficié d'intersections avec d'autres formes de privilèges, notamment le fait d'être homme et cisgenre, ainsi que d'avoir accès à la richesse familiale et aux études supérieures. Certaines et certains d'entre nous s'identifient également comme des personnes homosexuelles, féministes et migrantes. Nous nous sommes engagées dans ce travail en reconnaissant que ces identités produisent un certain nombre de limitations et d'angles morts, c'est pourquoi nous appelons toutes les personnes qui s'intéressent à la lutte contre le racisme en urbanisme à collaborer avec nous et à critiquer notre travail.", "Pour plus d’informations sur la façon dont nous informons notre position d’alliées et d’alliés, veuillez consulter le",
                                                  tags$a(href = "https://guidetoallyship.com/", "Guide d’allié", "d’Amélie Lamont.")))),
                                    br() )),
                           
                           fluidRow(
                             
                             
                             column(5, offset = 1,
                                    helpText(h3("Qu’y a-t-il dans ce guide?")),
                                    helpText(p(style="text-align: justify;", 
                                               
                                               h4("Ce guide tente de collecter et de conserver les contributions d’urbanistes, d’universitaires, d’artistes, d’écrivaines et d’écrivains, d’organisatrices et d’organisateurs ainsi que de praticiennes et praticiens noirs de diverses disciplines concernés par le processus d'aménagement de l'espace urbain. Les travaux énumérés ici représentent:"),
                                               h4(img(src='green_mark.png', width="20"),  "À la fois des préoccupations traditionnelles de l’urbanisme, telles que la politique du logement, la planification des transports et le design urbain, ainsi que des domaines plus interdisciplinaires comme la sociologie urbaine, l'histoire culturelle et les approches centrées sur les personnes noires dans le domaine de l’organisation et du développement communautaire. 
"),
                                               h4(img(src='green_mark.png', width="20"), "Une variété d'approches critiques et de nouvelles méthodologies utilisées pour décentrer la blancheur dans l'analyse des enjeux urbains."),
                                               h4(img(src='green_mark.png', width="20"), 
                                                  "Si la majorité des ressources sont des livres ou des articles de revues, la base de données vise également à inclure divers médias tels que des films, des podcasts, des essais sous forme de livre papier et numérique. Divers médias tels que des films et des blogs, ainsi que des livres et des journaux académiques.")))
                             ),
                             column(5,
                                    helpText(h3("Ce qu’il n’y as PAS dans ce guide")),
                                    helpText(p(style="text-align: justify;", 
                                               
                                               h4(img(src='red_mark.png', width="20"), "Littérature non-noir.e.s"),
                                               h4(
                                                 "Essentiellement, ce guide n'inclut pas d’autrices et auteurs écrivant sur les personnes noires, qui ne soient pas elles-mêmes noires. Il existe une mine de connaissances importantes de la part de chercheuses, chercheurs et d’urbanistes non-noirs sur les pratiques racistes enracinées dans l'urbanisme, tels que le redlining, l’embourgeoisement et la « revitalisation urbaine ». Ces chercheuses et chercheurs ont également documenté les victoires des mouvements sociaux urbains et des activités de sensibilisation au sein des communautés noires. Bien que ces ressources soient essentielles à une compréhension globale de la dynamique historique, sociale et économique des villes, ce guide est plutôt destiné à mettre en évidence spécifiquement les idées et les œuvres de créatrices et créateurs noirs."))),
                                    
                             )),
                           br(),
                           br(),
                           fluidRow(
                             
                             
                             column(10, offset = 1,
                                    helpText(h3("Remerciements")),
                                    helpText(p(style="text-align: justify;", 
                                               h4("Ce guide est avant tout redevable aux chercheuses, chercheurs, autrices, auteurs, créatrices et créateurs listés ici. C'est aussi le résultat de l'effort collectif d'étudiantes, d’étudiants, de graduées, de gradués et d'autres personnes, qui ont consacré leur temps et leurs ressources à changer la façon dont nous apprenons ce qu'est l’urbanisme ainsi que de mettre en lumière des urbanistes et aménagistes qui ont été marginalisés au sein du canon urbanistique. Enfin, nous tenons à saluer les efforts des individus et des organisations qui ont également créé des listes de ressources complémentaires et des guides sur la lutte contre le racisme et les personnes noires en urbanisme et qui ont tous aidé à la réalisation de ce projet.")))
                             ),
                             br(),
                             
                             
                             br() ) ) ,
                  
                  
                  tabPanel("Autres ressources",
                           br(), br(),
                           tags$a(href='https://bvotc.shinyapps.io/Guide-fr/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                           fluidRow(
                             column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                                    img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                           ),
                           br(), br(),
                           fluidRow(
                             column(10, offset = 1,
                                    helpText(p(style="text-align: justify;",
                                               h4("Vous trouverez ici une sélection non exhaustive d'organisations d'urbanistes dirigées par des Noirs, des recommandations de sources médiatiques et une liste de ressources complémentaires." 
                                                  
                                                  
                                               ))),
                                    helpText(p(style="text-align: justify;", 
                                               h4("Si vous souhaitez contribuer à cette liste,  veuillez utiliser notre", 
                                                  tags$a(href = "https://docs.google.com/forms/d/e/1FAIpQLSdoOvTsbXbaVlFZlkoUM_s3rY6dvebZrXZJn5OMt0YlLa0JQA/viewform", "formulaire pour les ressources supplémentaires"),
                                                  "ou  envoyez-nous un courriel à",
                                                  
                                                  tags$a(href = "mailto: bvotc.guide@gmail.com", "bvotc.guide@gmail.com."),
                                                  br(),
                                                  
                                                  
                                                  helpText(p(style="text-align: justify;", 
                                                             h6( " Remarque: Merci de suivre ces comptes. Toutefois, n'oubliez pas de ne pas submerger les professionnelles et professionnels noirs avec des demandes de collaboration ou d'autres questions à moins que vous ne les connaissiez personnellement ou si vous avez une relation de travail établie en particulier si vous êtes une personne blanche." )) )
                                                  
                                               ))))),
                           
                           
                           br(),
                           
                           fluidRow(
                             column(10, offset = 1,
                                    wellPanel(
                                      tags$div(align = 'left',
                                               
                                               h5(checkboxGroupInput(inputId = "language2",
                                                                     label = h3("Langue"),
                                                                     choices = c("Français", "Anglais"),
                                                                     selected = "Français"))
                                               
                                               # h5(  selectInput(inputId = "language2",
                                               #                  label = h3("Langue"),
                                               #                  choices = c("Tous", "Anglais", "Français"),
                                               #                  selected = "Français",
                                               #                  width = "25%"))
                                               
                                               # h5(radioButtons(inputId = "type2",
                                               #                 label = NULL,
                                               #                 choices = unique(data_AR$Type),
                                               #                 inline = FALSE)) 
                                      ))    ) ),
                           br(),
                           fluidRow(
                             column(10,offset = 1,
                                    h5(dataTableOutput("table_AR"))), 
                             column(6, offset = 3,  align= "center",
                                    
                                    a(h4("AJOUTER UNE ORGANISATION / UN MÉDIA", class = "btn btn-link btn-lg" , ), target = "_blank",
                                      href = "https://forms.gle/shRv5DrPB2NqNbAV8")
                             )), 
                           
                  ),
                  
                  
                  
                  tabPanel("Contactez-nous",
                           br(), br(),
                           tags$a(href='https://bvotc.shinyapps.io/Guide/',tags$img(src='Logo-Dark-Fresno-6.png', width = "50")),
                           fluidRow(
                             column(12, offset = 0, style='padding-left:0px; padding-right:0px; margin-left: -1.1em ; margin-right: -1.1em',
                                    img(src='MainPageBanner-Slimmest.png', width = '102.2%'))
                           ),
                           br(), br(), 
                           
                           
                           fluidRow(
                             column(6, offset = 3,
                                    helpText(p(style="text-align: justify;",
                                               
                                               h4("Si vous êtes tombé sur des écrits acaémiques, des articles web ou des vidéos créés par un urbaniste, un activiste ou un urbaniste noir, vous pouvez utiliser notre formulaire pour ajouter au guide:"))))),
                           
                           fluidRow(column(6, offset = 3,  align= "center",
                                           a(h4("AJOUTER AU GUIDE", class = "btn btn-default btn-lg action-button" , ), target = "_blank",
                                             href = "https://forms.gle/ZXuqpaCmrXLDZWRs9")
                           )),                       
                           br(),  
                           fluidRow(
                             column(6, offset = 3,
                                    helpText(p(style="text-align: justify;",
                                               h4("Cet inventaire (non-exhaustif)de perspectives noires sur le thème de l'urbanisme est à 100 % crowdsourced [créé par des collaborateurs bénévoles] et nous accueillons toutes les suggestions qui correspondent aux critères de la page « Mode d'emploi »."),
                                               h4("Si vous souhaitez ajouter une organisation d'urbanistes dirigée par des personnes Noires ou une source médiatique générale (par exemple un blog, un balado, un compte Twitter, une liste de ressources) à la page « Autres ressources », veuillez utiliser ce formulaire:"))))),
                           
                           fluidRow(column(6, offset = 3,  align= "center",
                                           
                                           a(h4("AJOUTER À LA PAGE « AUTRES RESSOURCES »", class = "btn btn-default btn-lg action-button" , ), target = "_blank",
                                             href = "https://forms.gle/shRv5DrPB2NqNbAV8")
                           )), 
                           
                           br(),   
                           
                           fluidRow(
                             column(6, offset = 3,
                                    helpText(p(style="text-align: justify;",
                                               
                                               h4("Pour signaler tout lien brisé, nous faire savoir comment nous pouvons améliorer le guide, ou encore pour collaborer ou rejoindre l'équipe, veuillez nous envoyer un courriel à ", tags$a(href = "mailto: bvotc.guide@gmail.com,", "bvotc.guide@gmail.com"), "ou nous contacter sur",  tags$a(href = "https://www.instagram.com/blackvoicesonthecity/", "Instagram"), "ou",
                                                  tags$a(href = "https://twitter.com/bvotcguide", "Twitter.")),
                                               br(), br()
                                    ) )))
                           
                           
                           
                  ) 
                  
                  
                  
                  
                  
                  
                ) ,
                
                
                
                fluidRow(
                  
                  column(12, offset = 0,  align= "center",
                         style = 'text-align: center',
                         br(), br(),
                         tags$a(href='https://twitter.com/bvotcguide',tags$img(src='Icon-Twitter.png', width = "40")),
                         tags$a(href='https://www.instagram.com/blackvoicesonthecity/',tags$img(src='Icon-IG.png', width = "40")),
                         tags$a(href="mailto: bvotc.guide@gmail.com", tags$img(src='Icon-Email.png', width = "40")) ),
                  br(),
                  column(6, offset = 3,  align= "center",
                         a(h4("AJOUTER AU GUIDE", class = "btn btn-link" ), target = "_blank",
                           href = "https://forms.gle/ZXuqpaCmrXLDZWRs9"),
                         br(), br(),
                         
                         
                         ## prevent disconnection
                         tags$head(
                           HTML(
                             "
                              <script>
                              var socket_timeout_interval
                              var n = 0
                              $(document).on('shiny:connected', function(event) {
                              socket_timeout_interval = setInterval(function(){
                              Shiny.onInputChange('count', n++)
                              }, 15000)
                              });
                              $(document).on('shiny:disconnected', function(event) {
                              clearInterval(socket_timeout_interval)
                              });
                              </script>
                              " ) ),
                         
                         textOutput("keepAlive"),
                         tags$head(tags$style("#keepAlive{color: white; }" ) )
                  ) )
                
                
) 


### Server ###
server <- function(input, output) {
  
  
  
  
  output$table <- DT::renderDataTable({
    
    validate(
      need(input$location != "", "Veuillez sélectionner une région."
      ))
    
    if (input$location == "Tous" ){
      data <- data %>%
        filter(`Tous` %in% input$keyword |
                 `Architecture et design urbain` %in% input$keyword |
                 `Cartographie et SIG` %in% input$keyword |
                 `Crime, police et surveillance` %in% input$keyword |
                 `Culture, “placemaking” et géographies noires` %in% input$keyword |
                 `Développement et embourgeoisement` %in% input$keyword |
                 `Durabilité, environnement et santé` %in% input$keyword |
                 `Espaces publics et parcs` %in% input$keyword |
                 `Histoire urbaine` %in% input$keyword |
                 `Logement social et coopératives` %in% input$keyword |
                 `Organisation communautaire et participation publique` %in% input$keyword |
                 `Perspectives noires sur la pratique et l’éducation en urbanisme` %in% input$keyword |
                 `Politique municipale et gouvernance` %in% input$keyword |
                 `Politiques territoriales, propriété et colonialisme` %in% input$keyword |
                 `Race et justice sociale` %in% input$keyword |
                 `Ségrégation et “redlining”` %in% input$keyword |
                 `Transport et mobilité` %in% input$keyword |
                 `Urbanisme féministe et queer` %in% input$keyword,
               item_format_2 %in% input$type,
               Langue %in% input$language,
               Année >= input$years[1] & Année <= input$years[2])}
    
    
    else {
      data <- data %>%
        filter(`Tous` %in% input$keyword |
                 `Architecture et design urbain` %in% input$keyword |
                 `Cartographie et SIG` %in% input$keyword |
                 `Crime, police et surveillance` %in% input$keyword |
                 `Culture, “placemaking” et géographies noires` %in% input$keyword |
                 `Développement et embourgeoisement` %in% input$keyword |
                 `Durabilité, environnement et santé` %in% input$keyword |
                 `Espaces publics et parcs` %in% input$keyword |
                 `Histoire urbaine` %in% input$keyword |
                 `Logement social et coopératives` %in% input$keyword |
                 `Organisation communautaire et participation publique` %in% input$keyword |
                 `Perspectives noires sur la pratique et l’éducation en urbanisme` %in% input$keyword |
                 `Politique municipale et gouvernance` %in% input$keyword |
                 `Politiques territoriales, propriété et colonialisme` %in% input$keyword |
                 `Race et justice sociale` %in% input$keyword |
                 `Ségrégation et “redlining”` %in% input$keyword |
                 `Transport et mobilité` %in% input$keyword |
                 `Urbanisme féministe et queer` %in% input$keyword,
               item_format_2 %in% input$type,
               Année >= input$years[1] & Année <= input$years[2],
               Langue %in% input$language,
               Region %in% input$location
        ) 
    }
    
    
    datatable(data, options = list(autoWidth = TRUE,
                                   scrollX=TRUE,
                                   
                                   columnDefs = list(
                                     list(targets=c(1), visible=TRUE, width = '14%'),
                                     list(targets=c(2), visible=TRUE, width='8%'),
                                     list(targets=c(3), visible=TRUE, width='33%'),
                                     list(targets=c(4), visible=TRUE, width='5%'),
                                     list(targets=c(5), visible=TRUE, width='12%'),
                                     list(targets=c(6), visible=TRUE, width='15%'),
                                     list(targets=c(7), visible=TRUE, width='13%'),
                                     list(targets = c(0,8:ncol(data)), visible = FALSE)),
                                   pageLength = 10),
              escape = FALSE #  makes HTML entities in the table not escaped (allows hyperlinks in table)
    )
  })  
  
  
  
  output$table_AR <- DT::renderDataTable({
    
    
    data_AR <- data_AR %>%
      
      filter(Langue %in% input$language2 ) 
    
    # if (input$language2 == "Tous"){
    #   
    # data_AR <- data_AR }
    
    # else {
    #   data_AR <- data_AR %>%
    # 
    # filter(Langue == input$language2 ) }
    
    
    
    
    
    datatable(data_AR,
              
              escape = FALSE,
              rownames= FALSE,
              options = list( autoWidth = FALSE,
                              scrollX=TRUE,
                              pageLength = 20,
                              columnDefs = list(list(targets=c(1), visible=TRUE, width = '32%'),
                                                list(targets=c(2), visible=TRUE, width='22%'),
                                                list(targets=c(3), visible=TRUE, width='10%'),
                                                list(targets=c(4), visible=TRUE, width='34%'),
                                                
                                                list(targets= c(0), visible=FALSE)
                                                
                              ) )
    )
  })
  
  
  ##needed to prevent disconnection  
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  
}

### Run the application ###
shinyApp(ui = ui, server = server)

