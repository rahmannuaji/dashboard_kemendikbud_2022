library(shiny) # Basic Shiny R
library(shinydashboard) # Shiny Dashboard
library(shinydashboardPlus)
library(tidyverse) # Data preprocessing / wrangling
library(ggplot2) # Graph
library(summarytools) # Summary
library(readxl) # Read excel data
library(DT)
library(dplyr)
library(dashboardthemes)
library(bslib)
library(fmsb)
library(plotly)
library (scales)
library(echarts4r)
library(leaflet)
library(spreadr)

#DATA
#setwd("D:/Kuliah/SEM 4/Sistem Informasi Manajemen/Final Project")
database<-read_excel("database.xlsx",sheet = "gambaran umum")
lulusan<-read_excel("database.xlsx",sheet = "lulusan")
putus<-read_excel("database.xlsx",sheet = "putus sekolah")
putuskelas<-read_excel("database.xlsx",sheet = "putus per kelas")
colnames(putuskelas)<-c("Provinsi","Tahun","I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII",
                        "SMK X","SMK XI","SMK XII","SMK XIII")
a <-read_xlsx("Indo_Coor.xlsx",sheet = 3) 
b <-read_excel("database.xlsx", sheet= 10)

sarpras<-read_excel("database.xlsx",sheet = "sarpras")
listrik<-sarpras[,4:5]
internet<-sarpras[,6:7]
lab<-read_excel("database.xlsx",sheet = "kondisi lab")
kelas<-read_excel("database.xlsx",sheet = "ruang kelas")
air<-read_excel("database.xlsx",sheet = "sumber air")
toilet<-read_excel("database.xlsx",sheet = "toilet")
uks<-read_excel("database.xlsx",sheet = "uks")
perpus<-read_excel("database.xlsx",sheet = "perpus")
#FOR MAP CHART


#UI ELEMENT



header <- dashboardHeader(
  title = tagList(
    span(class = "logo-lg", "Pendidikan Indonesia"), 
    img(
      src = "https://ue.ucdavis.edu/sites/g/files/dgvnsk4711/files/UE%20Logo-01_1.png",
      style = "width: 35px"
    ),
    titleWidth = 350),
  userOutput("user"))
sidebar <- dashboardSidebar(
  width = 210,
  sidebarMenu(
    menuItem("Home", 
             tabName = "Home", 
             icon = icon("home")),
    menuItem("Get to Know", 
             tabName = "get to know",
             icon = icon("question"),
             menuSubItem("Tentang Kemendikbud",
                         tabName = "tentang"),
             menuSubItem("Fungsi Kemendikbud",
                         tabName = "fungsi"),
             menuSubItem("Sejarah Kemendikbud",
                         tabName = "sejarah")),
    menuItem("Gambaran Umum", 
             tabName = "umum", 
             icon = icon("stats",lib = "glyphicon")),
    menuItem("Kondisi Pendidikan",
             tabName = "kondisi",
             icon = icon("sort-by-attributes",lib = "glyphicon")),
    menuItem("Database", 
             tabName = "Database", 
             icon = icon("list-alt",lib = "glyphicon"),
             menuSubItem("SD",
                         tabName = "datasd"),
             menuSubItem("SMP",
                         tabName = "datasmp"),
             menuSubItem("SMA",
                         tabName = "datasma"),
             menuSubItem("SMK",
                         tabName = "datasmk")),
    menuItem("Author",
             tabName = "penyusun",
             icon = icon("user"))
  ),tags$head(tags$style(HTML('* {font-family: "Montserrat"};')))
)
body<-dashboardBody(
  tags$head(tags$style(HTML('* {font-family: "Montserrat"};'))),
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #005689;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #146C94;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #005689;
                                font-color: #FAEEE0;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #4b85a6;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #146C94;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #4b85a6;
                                color: #FAEEE0;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #fffff;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #fffff;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #fffff;
                                }
                                
                                '))),
  tabItems(
    tabItem(tabName = "Home",
            fluidRow(
              div(strong("Welcome to Dashboard Pendidikan Indonesia"),
                  style="text-align: center;font-size: 250%"),
              br(),
              carousel(width = 12,
                       id = "mycarousel",
                       carouselItem(
                         tags$img(src = "https://cdn.idntimes.com/content-images/post/20180430/a9-1b7c835cfd7c8a3c8ec2a0ddc23b39c7.jpg",
                                  style ="width: 100%; height: auto;")
                       ),
                       carouselItem(
                         tags$img(src = "https://www.goodnewsfromindonesia.id/uploads/post/large-kids-3659890-1280-9dde230f0a67fb7eb3498623fe84e10d.jpg",
                                  style ="width: 100%; height: auto;")
                       )
                       ,
                       carouselItem(
                         tags$img(src = "https://assets-a1.kompasiana.com/items/album/2020/07/26/01-potret-pendidikan-di-indonesia-bangunsekolah-com-5f1ccb22097f360aad164585.jpg?t=o&v=1200",
                                  style ="width: 100%; height: auto;")
                       )
                       ,
                       carouselItem(
                         tags$img(src = "https://www.goodnewsfromindonesia.id/uploads/post/large-shutterstock-1672551346-951cc423ecb3f8f93eec7792bba6ee12.jpg",
                                  style ="width: 100%; height: auto;")
                       ),
                       carouselItem(
                         tags$img(src = "https://1.bp.blogspot.com/-Ae94-BYorYE/YNfTwm0z-3I/AAAAAAAAWrE/XkB47qWu7u8YlXh2EHVgqnQnyRXminfjgCLcBGAsYHQ/s700/Kemendikbud%2BRistek%252C%2BKepala%2BSekolah%2Bdan%2BGuru%2BWajib%2BTahu%2BSaat%2BPTM%2BTerbatas%2BDimulai.jpg",
                                  style ="width: 100%; height: auto;")
                       ),
                       
                       br()     
              ),
              div(h3(strong("Latar Belakang"),
                     style="text-align: center;")),
              box(title = NULL,
                  width=12,
                  p("Pendidikan di Indonesia merupakan bidang penting dalam pembangunan negara dengan sistem yang 
                  meliputi berbagai tingkatan. Saat ini, peran pendidikan telah berubah akibat globalisasi, di 
                  mana generasi muda dituntut untuk memiliki pengetahuan, keterampilan, dan moral yang baik. 
                    Meskipun telah ada perbaikan, kualitas pendidikan di Indonesia masih menghadapi tantangan, 
                    seperti kesenjangan antara sekolah di perkotaan dan pedesaan dalam hal fasilitas, buku teks, 
                    dan tenaga pendidik berkualitas. ",style="text-align:justify;"),
                  br(),
                  p("Selain itu, kualitas tenaga pendidik juga menjadi permasalahan serius yang mempengaruhi kualitas
                    pendidikan. Upaya peningkatan pelatihan dan pengembangan guru terus dilakukan, termasuk peningkatan 
                    insentif untuk menarik dan mempertahankan guru berkualitas. Pemerintah Indonesia telah menyadari 
                    pentingnya meningkatkan kualitas pendidikan dan telah melaksanakan program dan kebijakan yang relevan. 
                    Namun, perbaikan kualitas pendidikan membutuhkan waktu, sumber daya, dan komitmen berkelanjutan dari 
                    berbagai pihak, termasuk pemerintah, lembaga pendidikan, tenaga pendidik, orang tua, dan masyarakat. 
                    Oleh karena itu, penelitian lebih lanjut mengenai pendidikan di Indonesia dapat memberikan wawasan yang 
                    lebih dalam dan berkontribusi pada pemecahan permasalahan yang ada.",style="text-align:justify;")
              ))),
    tabItem(tabName = "tentang",
            div(imageOutput("logo"),
                style="text-align: center;"),
            box(title = strong("VISI"),
                width = 12,
                solidHeader = T,
                style="text-align:justify;",
                p("Kementerian Pendidikan dan Kebudayaan mendukung Visi dan Misi Presiden 
                  untuk mewujudkan Indonesia Maju yang berdaulat, mandiri, dan berkepribadian 
                  melalui terciptanya Pelajar Pancasila yang bernalar kritis, kreatif, mandiri, 
                  beriman, bertakwa kepada Tuhan Yang Maha Esa, dan berakhlak mulia, 
                  bergotong royong, dan berkebinekaan global.")),br(),
            box(title = strong("MISI"),
                width = 12,
                solidHeader = T,
                style="text-align:justify;",
                p("1. Mewujudkan pendidikan yang relevan dan berkualitas tinggi, merata dan 
                  berkelanjutan, didukung oleh infrastruktur dan teknologi."),
                p("2. Mewujudkan pelestarian dan pemajuan kebudayaan serta pengembangan 
                  bahasa dan sastra."),
                p("3. Mengoptimalkan peran serta seluruh pemangku kepentingan untuk 
                  mendukung transformasi dan reformasi pengelolaan pendidikan dan 
                  kebudayaan")),br(),
            box(title = strong("TUGAS"),
                width = 12,
                solidHeader = T,
                style="text-align:justify;",
                p("Kementerian Pendidikan dan Kebudayaan mempunyai tugas menyelenggarakan 
                  urusan pemerintahan di bidang pendidikan (pendidikan anak usia dini, 
                  pendidikan dasar, pendidikan menengah, dan pendidikan tinggi) serta 
                  pengelolaan kebudayaan untuk membantu Presiden dalam menyelenggarakan 
                  pemerintahan negara.")),br(),
            box(title = strong("SUSUNAN ORGANISASI"),
                width = 12,
                solidHeader = T,
                style="text-align:justify;",
                p("Dalam rangka mengoptimalkan pelaksanaan tugas dan fungsi Kementerian Pendidikan dan Kebudayaan, 
                  Menteri Pendidikan dan Kebudayaan dalam memimpin pelaksanaan tugas dan fungsi kementerian, didukung 
                  oleh 9 (sembilan) unit Eselon I sebagai berikut:"), 
                p("1. Sekretariat Jenderal;"), 
                p("2. Direktorat Jenderal Guru dan Tenaga Kependidikan;"),
                p("3. Direktorat Jenderal Pendidikan Anak Usia Dini, Pendidikan Dasar dan Pendidikan Menengah;"), 
                p("4. Direktorat Jenderal Pendidikan Vokasi;"), 
                p("5. Direktorat Jenderal Pendidikan Tinggi;"), 
                p("6. Direktorat Jenderal Kebudayaan;"), 
                p("7. Inspektorat Jenderal;"), 
                p("8. Badan Penelitian dan Pengembangan dan Perbukuan; dan"), 
                p("9. Badan Pengembangan dan Pembinaan Bahasa."))
    ),
    tabItem(tabName = "fungsi",
            div(h2(strong("Fungsi Kemendikbud Ristek"),
                   style="text-align: center;")),
            div(imageOutput("logo2"),
                style="text-align: center;"),
            box(title = NULL,
                width = 12,
                solidHeader = T,
                style="text-align:justify;",
                p("1. Perumusan dan penetapan kebijakan di bidang pendidik dan tenaga kependidikan, pendidikan anak usia dini, 
                pendidikan dasar, pendidikan menengah, pendidikan vokasi, pendidikan tinggi, dan pengelolaan kebudayaan;"), 
                p("2. Pelaksanaan kebijakan di bidang pengendalian formasi pendidik, pemindahan pendidik, dan 
                  pengembangan karir pendidik, serta pemindahan pendidik dan tenaga kependidikan lintas daerah provinsi;"),
                p("3. Penetapan standar nasional pendidikan dan kurikulum nasional pendidikan menengah, pendidikan dasar, pendidikan 
                  anak usia dini, dan pendidikan nonformal;"), 
                p("4. Pelaksanaan kebijakan di bidang pendidikan tinggi;"), 
                p("5. Pelaksanaan fasilitasi pendidik dan tenaga kependidikan dan penyelenggaraan pendidikan anak usia 
                  dini, pendidikan dasar, pendidikan menengah, pendidikan vokasi, dan pendidikan tinggi, serta pengelolaan 
                  kebudayaan;"), 
                p("6. Pelaksanaan penelitian dan pengembangan di bidang pendidik dan tenaga kependidikan, pendidikan anak 
                  usia dini, pendidikan dasar, pendidikan menengah, pendidikan vokasi, pendidikan tinggi, dan pengelolaan 
                  kebudayaan;"), 
                p("7. Pelaksanaan kebijakan di bidang pelestarian cagar budaya dan pemajuan kebudayaan;"), 
                p("8. Pelaksanaan kebijakan di bidang pembinaan perfilman nasional;"), 
                p("9. Pelaksanaan pengembangan, pembinaan, dan pelindungan bahasa dan sastra Indonesia;"),
                p("10. Pelaksanaan pengelolaan sistem perbukuan;"),
                p("11. Pelaksanaan bimbingan teknis dan supervisi atas pelaksanaan urusan Kementerian di daerah;"),
                p("12. Koordinasi pelaksanaan tugas, pembinaan, dan pemberian dukungan administrasi kepada seluruh unsur 
                  organisasi di lingkungan Kementerian;"),
                p("13. Pengelolaan barang milik/kekayaan negara yang menjadi tanggung jawab Kementerian;"),
                p("14. Pengawasan atas pelaksanaan tugas di lingkungan Kementerian; dan"),
                p("15. Pelaksanaan dukungan substantif untuk mendukung pencapaian tujuan dan sasaran strategis Kementerian."))
    ),
    tabItem(tabName = "sejarah",
            div(h2(strong("Sejarah Kemendikbud Ristek"),
                   style="text-align: center;")),
            div(imageOutput("logo3"),
                style="text-align: center;"),
            box(title = "Sejarah",
                width = 12,
                style="text-align:justify;",
                p("Kementerian Pendidikan dan Kebudayaan 
                  dahulu bernama “Departemen Pengajaran” 
                  dibentuk pertama kali 19 Agustus 1945 dan 
                  sampai saat ini telah mengalami enam kali 
                  perubahan nomenklatur. Departemen 
                  Pengajaran bertahan selama tiga tahun 
                  (1945-1948) lalu diganti dengan dengan 
                  Departemen Pendidikan dan Kebudayaan 
                  pada dua jangka waktu, yaitu 1948-1955 dan 
                  1956-1999. Di tengah kedua waktu tersebut, 
                  organisasi ini pernah menjadi Departemen 
                  Pengajaran, Pendidikan, dan Kebudayaan 
                  pada 1955-1956."), br(),
                p("Nomenklatur berikutnya adalah “Departemen 
                  Pendidikan Nasional” (1999-2009), disingkat 
                  Depdiknas yang merupakan kementerian 
                  dalam Pemerintah Indonesia yang 
                  membidangi urusan pendidikan. Lalu pada 
                  2009-2011, Depdiknas berubah lagi menjadi 
                  Kementerian Pendidikan Nasional dan 
                  dipimpin oleh seorang Menteri Pendidikan 
                  Nasional (Mendiknas) yang sejak tanggal 22 
                  Oktober 2009 sampai dengan 20 Oktober 
                  2014 dijabat oleh Mohammad Nuh."), br(),
                p("Mohammad Nuh menjabat sebagai menteri 
pendidikan pada dua nomenklatur. Pertama 
kali dilantik, ia menjabat sebagai Menteri Pendidikan Nasional, dan pada 2011 menjadi 
Menteri Pendidikan dan Kebudayaan 
(Mendikbud), karena nomenklatur 
Kemdiknas berubah menjadi Kementerian 
Pendidikan dan Kebudayan 
(Kemendikbud). Nomenklatur 
Kemendikbud bertahan hingga saat ini."), br(),
                p("Setelah Mohammad Nuh, posisi 
Mendikbud dijabat oleh Anies Rasyid 
Baswedan (27 Oktober 2014 -- 27 Juli 2016). 
Masih di era Kabinet Gotong Royong 
Presiden Joko Widodo dan Wakil Presiden 
Jusuf Kalla, sejak tanggal 27 Juli 2016 
Muhadjir Effendy menggantikan Anies 
Rasyid Baswedan sebagai Mendikbud."), br(),
                p("Baru pada 23 Oktober 2019, Presiden 
Jokowi melantik Nadiem Anwar Makarim 
sebagai Mendikbud di era Kabinet 
Indonesia Maju Presiden Joko Widodo dan 
Wakil Presiden Ma’ruf Amin. Hingga saat 
ini Mendikbud Anwar Makarim 
melanjutkan estafet perjuangan bidang 
pendidikan dan kebudayaan bagi bangsa 
Indonesia sesuai dengan amanat 
Undang-Undang Dasar 1945."))
            
    ),
    tabItem(tabName = "umum",
            div(h2(strong("Gambaran Umum Pendidikan Indonesia Pasca Pandemi"),
                   style="text-align: center;")),
            box(width = 12,
                leafletOutput("mapjml", 
                              height=500) ),
            br(),
            fluidRow(
              box(title = "Provinsi",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("prov","Pilih Provinsi", 
                              unique(database$`Provinsi`),
                              selected = "Indonesia")),
              box(
                title = "Tahun",width = 2,
                radioButtons(inputId = "tahun",
                             label=NULL,
                             choices = c('2021' , '2022'),
                             selected = '2021',
                             inline=TRUE)),  
              box(
                title = "Jenjang Pendidikan",
                width = 4,
                radioButtons(inputId = "jenjang",
                             label=NULL,
                             choices = c('SD' , 'SMP', 'SMA','SMK'),
                             selected = 'SD',
                             inline = TRUE),
                collapsible = TRUE),
              div(em("note : Pilih provinsi, tahun dan jenjang sekolah untuk menampilkan data"),style="text-align: center;"),
              br()),
            fluidRow(
              infoBoxOutput(outputId = "jmlsiswa",
                            width = 4),
              infoBoxOutput(outputId = "jmlguru",
                            width = 4),
              infoBoxOutput(outputId = "jmlsekolah",
                            width = 4),
              box(
                echarts4rOutput("gender"),
                collapsible = TRUE,
                width = 5
              ),
              box(
                plotOutput("lulusan"),
                collapsible = TRUE,
                width = 7
              ),
              box(
                plotOutput("putus"),
                collapsible = TRUE,
                width = 7
              ),
              box(
                echarts4rOutput("putusgen"),
                collapsible = TRUE,
                width = 5
              )
            )),
    tabItem(tabName = "kondisi",
            
            fluidRow(
              box(title = "Provinsi",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("prov2","Pilih Provinsi", 
                              unique(database$`Provinsi`),
                              selected = "Indonesia")),
              box(
                title = "Tahun",
                width = 2,
                radioButtons(inputId = "tahun2",
                             label=NULL,
                             choices = c('2021' , '2022'),
                             selected = '2021',
                             inline = TRUE),
                collapsible = TRUE),
              box(
                title = "Jenjang Pendidikan",
                width = 4,
                radioButtons(inputId = "jenjang2",
                             label=NULL,
                             choices = c('SD' , 'SMP', 'SMA','SMK'),
                             selected = 'SD',
                             inline = TRUE),
                collapsible = TRUE),
              div(em("note : Pilih provinsi, tahun dan jenjang sekolah untuk menampilkan data"),
                  style="text-align: center;"),
              br()),
            fluidRow(
              infoBoxOutput(outputId = "melekhuruf",
                            width = 6),
              infoBoxOutput(outputId = "lamasekolah",
                            width = 6),
              box(
                plotOutput("akreditasi"),
                collapsible = TRUE,
                width = 8
              ),
              box(
                plotOutput("guru"),
                collapsible = TRUE,
                width = 4
              ),
              box(
                plotOutput("putuskelas", 
                           height = 400),
                collapsible = TRUE,
                width = 12
              )
            ),
            fluidRow(
              div(h3(strong("Kondisi Sarana dan Prasarana Pendidikan di Indonesia")),
                  style="text-align: center;"),
              br()),
            fluidRow(
              box(
                echarts4rOutput("listrik",
                                height = 280),
                collapsible = TRUE,
                width = 4
              ),
              box(
                echarts4rOutput("internet",
                                height = 280),
                collapsible = TRUE,
                width = 4
              ),
              box(
                echarts4rOutput("air",
                                height = 280),
                collapsible = TRUE,
                width = 4
              )
            ),
            fluidRow(
              box(
                plotOutput("kelas",
                           height = 200),
                collapsible = T,
                width =6
              ),
              box(
                plotOutput("uks",
                           height = 200),
                collapsible = T,
                width =6
              ),
              fluidRow(             
                box(
                  plotOutput("lab",
                             height = 200),
                  collapsible = T,
                  width =6
                ),
                box(
                  plotOutput("perpus",
                             height = 200),
                  collapsible = T,
                  width =6
                )
              )
            )
    ),
    tabItem(tabName = "datasd",
            DTOutput("esde"),
            width=12),
    tabItem(tabName = "datasmp",
            DTOutput("esempe")),
    tabItem(tabName = "datasma",
            DTOutput("esema")),
    tabItem(tabName = "datasmk",
            DTOutput("esemka")),
    tabItem(
      tabName = "penyusun",
      fluidRow(
      div(h1(strong("DASHBOARD")), 
          style = "text-align: center;"),
      div(h2(strong("PERSEBARAN PENDIDIKAN DI INDONESIA")), 
          style = "text-align: center;"),
      br(),
      div(h2("Author"), 
          style = "text-align: center;"),
      br()),
      fluidRow(
        box(
          width = 6,
          status = NULL,
          div(imageOutput("adristy"), 
              style = "text-align:center;",
              style = "margin-bottom:-180px;"),
          br(),
          div(strong("Adristy Rizky Fahriyah"), 
              style = "text-align:center;font-family: 'Montserrat';"),
          div(strong("5003211082"), 
              style = "text-align: center;font-family: 'Montserrat';"),
          br(),
          div(strong("For More Information"),
              style = "=font-family: 'Montserrat', sans-serif"),
          div("Contact Us",style = "font-family: 'Montserrat', sans-serif"),
          br(),
          tags$a(href="https://www.linkedin.com/in/adristy-rizki-fahriyah-2846aa237/",
                 target="_blank","LinkedIn"),
          br(),
          tags$a(href="https://www.instagram.com/adristyrizki/",
                 target="_blank","Instagram")
        ),
        box(
          width = 6,
          status = NULL,
          div(imageOutput("aji"), 
              style = "text-align:center;",
              style = "margin-bottom:-180px;"),
          br(),
          div(strong("Rahmannuaji Satuhu"), 
              style = "text-align:center;font-family: 'Montserrat', sans-serif"),
          div(strong("5003211125"), 
              style = "text-align:center;font-family: 'Montserrat', sans-serif"),
          br(),
          div(strong("For More Information"),
              style = "font-family: 'Montserrat', sans-serif"),
          div("Contact Us",style = "font-family: 'Montserrat', sans-serif"),
          br(),
          tags$a(href="https://www.linkedin.com/in/rahmannuajisatuhu/",
                 target="_blank","LinkedIn"),
          br(),
          tags$a(href="https://www.instagram.com/rhmannuaji.s/",
                 target="_blank","Instagram")
        )),
      fluidRow(br(),
        div(h3(strong("Sistem Informasi Manajemen")), 
            style = "text-align: center;"),
        div(("Departemen Statistika"), 
            style = "text-align: center;"),
        div(("Fakultas Sains dan Analitika Data"), 
            style = "text-align: center;"),
        div(("Institut Teknologi Sepuluh Nopember"), 
            style = "text-align: center;"),
        div(("2022/2023"), 
            style = "text-align: center;")
      )
    )
  ))
ui <- shinyUI(dashboardPage(header = header,
                            sidebar = sidebar,
                            body = body
))
server <- function(input,output,session){
  output$logo<-renderImage({
    list(src="www/logo.png",
         height = 360,
         width = 640,
         align="center")
  },deleteFile = F)
  
  output$logo2<-renderImage({
    list(src="www/logo.png",
         height = 360,
         width = 640,
         align="center")
  },deleteFile = F)
  
  output$logo3<-renderImage({
    list(src="www/logo.png",
         height = 360,
         width = 640,
         align="center")
  },deleteFile = F)
  
  #GAMBARAN UMUM
  metrik<-reactive({database%>%
      filter(`Provinsi`==input$prov)%>%
      filter(`Tahun`==input$tahun)%>%
      filter(`Jenjang`==input$jenjang)})
  
  output$mapjml<-renderLeaflet({
    # try addAwesomeMarkers with popupOptions
    icon.glyphicon <- makeAwesomeIcon(
      icon= 'university', markerColor = 'white',
      iconColor = 'green', library = 'fa'
    )
    pop.oke <- paste0("<div style='font-size: 18px; font-weight: bold;font-family: Montserrat'>Jumlah Sekolah</div>",
                      "<br><div style='font-size: 15px;font-family: Montserrat'>SD:  ",b$SD,"</div>",
                      "<div style='font-size: 15px;font-family: Montserrat'>SMP:  ",b$SMP,"</div>",
                      "<div style='font-size: 15px;font-family: Montserrat'>SMK:  ",b$SMA,"</div>",
                      "<div style='font-size: 15px;font-family: Montserrat'>SMA:  ",b$SMK,"</div>",
                      "<br>","<div style='font-size: 15px;font-family:Montserrat'> Provinsi:  ",b$Provinsi,"</div>")
    
    
    y <-  leaflet(pop.oke) %>%
      setView(lng=113.921327,lat=-0.789275,zoom=5.1) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = T)) %>%
      addProviderTiles(providers$OpenTopoMap, group= "Open Topo Map", options = providerTileOptions(noWrap = T)) %>%
      addProviderTiles(providers$Stamen.Watercolor, group= "Stamen Watercolor", options = providerTileOptions(noWrap = T)) %>%
      addProviderTiles(providers$CartoDB.Positron, group ="Carto DB", options = providerTileOptions(noWrap = T)) %>%
      addProviderTiles(providers$NASAGIBS.ModisTerraTrueColorCR, group="Nasa Gibs", options = providerTileOptions(noWrap = T))%>%
      
      
      addAwesomeMarkers(
        lng = a$Longitude, 
        lat = a$Latitude,
        icon = icon.glyphicon,
        popup = pop.oke,
        popupOptions = popupOptions(maxWidth = 1000, closeOnClick = TRUE)
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map","Open Topo Map","Stamen Watercolor","Carto DB","Nasa Gibs"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  output$jmlsiswa<-renderValueBox({
    valueBox(value = metrik()$`Jumlah Siswa`,
             subtitle = "Jumlah Siswa",
             icon = icon("users"),
             color = "green",
             width = 4)
  })
  
  output$jmlguru<-renderValueBox({
    valueBox(value = metrik()$`Jumlah Guru (Laki-Laki)` + metrik()$`Jumlah Guru (Perempuan)`,
             subtitle = "Jumlah Guru",
             icon = icon("chalkboard-user"),
             color = "yellow",
             width = 4)
  })
  
  output$jmlsekolah<-renderValueBox({
    valueBox(value = metrik()$`Jumlah Sekolah`,
             subtitle = "Jumlah Sekolah",
             icon = icon("school"),
             color = "red",
             width = 4)
  })
  
  output$lulusan<-renderPlot({
    lulusan<-lulusan%>%
      filter(`Provinsi`==input$prov)%>%
      filter(`Jenjang`==input$jenjang)
    lulusan%>%ggplot(aes(x=`Tahun`,y=`Total`))+
      ggtitle("Perkembangan Jumlah Lulusan",
              subtitle = "Rentang tahun 2019 - 2022")+
      geom_line(color="green",size=2)+
      geom_point(color="#1C6758",size=2.5)+
      geom_text(family="Montserrat",aes(y=`Total`,label = `Total`),
                nudge_y = -1,nudge_x = -0.25,color="black")+
      theme(panel.background = element_rect(fill = "white"),
            legend.position = "top",
            axis.ticks.y = element_blank(),
            plot.title = element_text(family = "Montserrat",
                                      face="bold",
                                      size = 18,
                                      hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",
                                         face="plain",
                                         size = 12, 
                                         hjust = 0.5),
            text = element_text(family = "Montserrat")) 
  })
  
  output$putus<-renderPlot({
    putus<-putus%>%
      filter(`Provinsi`==input$prov)%>%
      filter(`Jenjang`==input$jenjang)
    putus%>%ggplot(aes(x=`Tahun`,y=`Total`))+
      ggtitle("Perkembangan Jumlah Putus Sekolah",
              subtitle = "Rentang tahun 2019 - 2022")+
      geom_line(color="#E06469",size=2)+
      geom_point(color="#B04759",size=2.5)+
      geom_text(family="Montserrat",aes(y=`Total`,label = `Total`),
                nudge_y = -1,nudge_x = -0.25,color="black")+
      theme(panel.background = element_rect(fill = "white"),
            legend.position = "top",
            axis.ticks.y = element_blank(),
            plot.title = element_text(family = "Montserrat",
                                      face="bold",
                                      size = 18, 
                                      hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",
                                         face="plain",
                                         size = 12, 
                                         hjust = 0.5),
            text = element_text(family = "Montserrat"))
  })
  
  output$gender<-renderEcharts4r({
    data<-database%>%
      filter(`Provinsi`==input$prov)%>%
      filter(`Tahun`==input$tahun)%>%
      filter(`Jenjang`==input$jenjang)
    
    lk<-round(data[,24]/(data[,24]+data[,25])*100,2)
    colnames(lk)="Persen"
    pr<-round(data[,25]/(data[,24]+data[,25])*100,2)
    colnames(pr)="Persen"
    dataa<-data.frame(c("Laki-laki","Perempuan"),rbind(lk,pr))
    
    
    gender = data.frame(gender=c("Laki-Laki", "Perempuan"), value=c(dataa[1,2], dataa[2,2]),
                        path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13',
                                 'path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z'))
    
    gender %>% 
      e_charts(gender) %>% 
      e_x_axis(splitLine=list(show = FALSE), 
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel= list(show=FALSE)) %>%
      e_y_axis(max=100, 
               splitLine=list(show = FALSE),
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel=list(show=FALSE)) %>%
      e_color(color = c('green','#eee')) %>%
      e_pictorial(value, symbol = path, z=10, name= 'realValue', 
                  symbolBoundingData= 100, symbolClip= TRUE) %>% 
      e_pictorial(value, symbol = path, name= 'background', 
                  symbolBoundingData= 100) %>% 
      e_labels(position = "bottom", offset= c(0, 8), 
               textStyle =list(fontSize= 13, fontFamily= 'Montserrat', 
                               fontWeight ='bold', 
                               color= '#1C6758'),
               formatter="{@[1]}% {@[0]}") %>%
      e_legend(show = FALSE) %>%
      e_title(text = "              Persentase Jumlah Siswa",
              subtext = "                                 Berdasarkan Jenis Kelamin")
  })
  
  output$putusgen<-renderEcharts4r({
    data<-database%>%
      filter(`Provinsi`==input$prov)%>%
      filter(`Tahun`==input$tahun)%>%
      filter(`Jenjang`==input$jenjang)
    
    lk<-round(data[,15]/(data[,15]+data[,16])*100,2)
    colnames(lk)="Persen"
    pr<-round(data[,16]/(data[,15]+data[,16])*100,2)
    colnames(pr)="Persen"
    dataa<-data.frame(c("Laki-laki","Perempuan"),rbind(lk,pr))
    
    
    gender = data.frame(gender=c("Laki-Laki", "Perempuan"), value=c(dataa[1,2], dataa[2,2]),
                        path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13',
                                 'path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z'))
    
    gender %>% 
      e_charts(gender) %>% 
      e_x_axis(splitLine=list(show = FALSE), 
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel= list(show=FALSE)) %>%
      e_y_axis(max=100, 
               splitLine=list(show = FALSE),
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel=list(show=FALSE)) %>%
      e_color(color = c('red','#eee')) %>%
      e_pictorial(value, symbol = path, z=10, name= 'realValue', 
                  symbolBoundingData= 100, symbolClip= TRUE) %>% 
      e_pictorial(value, symbol = path, name= 'background', 
                  symbolBoundingData= 100) %>% 
      e_labels(position = "bottom", offset= c(0, 8), 
               textStyle =list(fontSize= 13, fontFamily= 'Montserrat', 
                               fontWeight ='bold', 
                               color= '#F96666'),
               formatter="{@[1]}% {@[0]}") %>%
      e_legend(show = FALSE) %>%
      e_title(text = "        Persentase Jumlah Putus Sekolah",
              subtext = "                                 Berdasarkan Jenis Kelamin",
              position="center")
  })
  
  #KONDISI PENDIDIKAN
  
  
  metrik2<-reactive({database%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)%>%
      filter(`Jenjang`==input$jenjang2)})
  
  output$melekhuruf<-renderValueBox({
    valueBox(value = metrik2()$`Angka Melek Huruf`,
             subtitle = "Angka Melek Huruf (persen)",
             icon = icon("book"),
             color = "teal",
             width = 5)
  })
  
  output$lamasekolah<-renderValueBox({
    valueBox(value = metrik2()$`Rata-Rata Lama Sekolah`,
             subtitle = "Rata-Rata Lama Sekolah (tahun)",
             icon = icon("calendar"),
             color = "light-blue",
             width = 5)
  })
  
  output$akreditasi<-renderPlot({
    data<-database%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)%>%
      filter(`Jenjang`==input$jenjang2)
    dataa<-data.frame(c("Akreditasi-A","Akreditasi-B","Akreditasi-C","Tidak Terakreditasi","Belum Terakreditasi"),
                      t(data[,10:14]))
    colnames(dataa)<-c("Akreditasi","Jumlah")
    
    dataa%>%ggplot(aes(y=`Jumlah`, x=`Akreditasi`, fill=`Akreditasi`)) + 
      ggtitle("Perbandingan Jumlah Sekolah",
              subtitle = "Berdasarkan Akreditasi Sekolah")+ 
      geom_bar(stat='identity')+
      labs(x=NULL,y=NULL) + 
      theme(panel.background = element_rect(fill = "white"),
            legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(family = "Montserrat",face = "bold"),
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat")) + 
      geom_text(family="Montserrat",aes(y=`Jumlah`,label = `Jumlah`),hjust=0.5, vjust=-1,color="black")+
      scale_fill_manual(values=c("#495C83","#6E85B7","#B2C8DF","#A8A4CE","#C8B6E2"))
  })
  
  output$guru<-renderPlot({
    data<-database%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)%>%
      filter(`Jenjang`==input$jenjang2)
    tetap<-data[,20]/(data[,20]+data[,21])
    colnames(tetap)="Persen"
    tidaktetap<-data[,21]/(data[,20]+data[,21])
    colnames(tidaktetap)="Persen"
    dataa<-data.frame(c("Tetap","Tidak Tetap"),rbind(tetap,tidaktetap))
    colnames(dataa)<-c("Jenis Guru","Persen")
    hsize<-3
    dataa<- dataa%>%mutate(x=hsize)
    highlight_color <- "#0078AA"
    font_family<-"Montserrat"
    label_pie<-percent(dataa$Persen[1],accuracy = 0.01)
    ggplot(dataa, aes(x = hsize, y = `Persen`, fill = `Jenis Guru`)) +
      ggtitle("Persentase Guru Tetap",
              subtitle = "Berdasarkan Data Kemendikbud") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c(highlight_color, "#D7E9F7")) +
      xlim(c(0.2, hsize + 0.5)) + theme_void() + 
      theme(legend.position = "none",
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat")) +
      annotate(geom = "text",
               family = font_family,
               label = label_pie,
               fontface = "bold",
               color = highlight_color,
               size = 12,
               x = 0.3,
               y = 0)
  })
  
  output$putuskelas<-renderPlot({
    data<-putuskelas%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)
    if (input$jenjang2=="SD") {
      Kelas<-factor(c("I","II","III","IV","V","VI"),
                    levels = c("I","II","III","IV","V","VI"))
      data<-data.frame(Kelas,t(data[,3:8]))
    } else if (input$jenjang2=="SMP"){
      Kelas<-factor(c("VII","VIII","IX"),
                    levels = c("VII","VIII","IX"))
      data<-data.frame(Kelas,t(data[,9:11]))
    } else if (input$jenjang2=="SMA") {
      Kelas<-factor(c("X","XI","XII"),
                    levels = c("X","XI","XII"))
      data<-data.frame(Kelas,t(data[,12:14]))
    } else {
      Kelas<-factor(c("X","XI","XII","XIII"),
                    c("X","XI","XII","XIII"))
      data<-data.frame(Kelas,t(data[,15:18]))
    }
    colnames(data)<-c("Kelas","Jumlah")
    
    data%>%ggplot(aes(y=`Jumlah`, x=`Kelas`, fill=`Kelas`)) + 
      ggtitle("Perbandingan Putus Sekolah",
              subtitle = "Berdasarkan Grade/Kelas") +
      geom_bar(stat='identity')+
      labs(x=NULL,y=NULL) + 
      theme(panel.background = element_rect(fill = "white"),
            legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(family = "Montserrat",face = "bold"),
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat")) + 
      geom_text(family="Montserrat",aes(y=`Jumlah`,label = `Jumlah`),hjust=0.5, vjust=-1,color="black")+
      scale_fill_manual(values=c("#73A9AD","#76BA99","#90C8AC","#C4DFAA","#FFDCAE","#F5F0BB"))
  })
  
  output$listrik<-renderEcharts4r({
    listrik<-sarpras[,c(1:3,4:5)]
    listrik<-listrik%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)%>%
      filter(`Jenjang`==input$jenjang2)
    persen<-round(listrik[,4]/(listrik[,4]+listrik[,5])*100,2)
    persen
    kondisi<-"Ada"
    colnames(persen)<-"persentase"
    listrik<-cbind(listrik[1:3],persen,kondisi)
    path ='path://M349.4 44.6c5.9-13.7 1.5-29.7-10.6-38.5s-28.6-8-39.9 1.8l-256 224c-10 8.8-13.6 22.9-8.9 35.3S50.7 288 64 288H175.5L98.6 467.4c-5.9 13.7-1.5 29.7 10.6 38.5s28.6 8 39.9-1.8l256-224c10-8.8 13.6-22.9 8.9-35.3s-16.6-20.7-30-20.7H272.5L349.4 44.6z'
    listrik<-cbind(listrik,path)
    listrik
    listrik %>% 
      e_charts(kondisi) %>% 
      e_x_axis(splitLine=list(show = FALSE), 
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel= list(show=FALSE)) %>%
      e_y_axis(max=100, 
               splitLine=list(show = FALSE),
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel=list(show=FALSE)) %>%
      e_color(color = c('#FFD95A','#FBFFDC')) %>%
      e_pictorial(persentase, symbol = path, z=10, name= 'realValue', 
                  symbolBoundingData= 100, symbolClip= TRUE) %>% 
      e_pictorial(persentase, symbol = path, name= 'background', 
                  symbolBoundingData= 100) %>% 
      e_labels(position = "bottom", offset= c(0, 8), 
               textStyle =list(fontSize= 30, fontFamily= 'Montserrat', 
                               fontWeight ='bold', 
                               color= '#FFD95A'),
               formatter="{@[1]}% ") %>%
      e_legend(show = FALSE) %>%
      e_title("                    Listrik")
  })
  
  output$internet<-renderEcharts4r({
    internet<-sarpras[,c(1:3,6:7)]
    internet<-internet%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)%>%
      filter(`Jenjang`==input$jenjang2)
    persen<-round(internet[,4]/(internet[,4]+internet[,5])*100,2)
    persen
    kondisi<-"Ada"
    colnames(persen)<-"persentase"
    internet<-cbind(internet[1:3],persen,kondisi)
    path ='path://"M54.2 202.9C123.2 136.7 216.8 96 320 96s196.8 40.7 265.8 106.9c12.8 12.2 33 11.8 45.2-.9s11.8-33-.9-45.2C549.7 79.5 440.4 32 320 32S90.3 79.5 9.8 156.7C-2.9 169-3.3 189.2 8.9 202s32.5 13.2 45.2 .9zM320 256c56.8 0 108.6 21.1 148.2 56c13.3 11.7 33.5 10.4 45.2-2.8s10.4-33.5-2.8-45.2C459.8 219.2 393 192 320 192s-139.8 27.2-190.5 72c-13.3 11.7-14.5 31.9-2.8 45.2s31.9 14.5 45.2 2.8c39.5-34.9 91.3-56 148.2-56zm64 160a64 64 0 1 0 -128 0 64 64 0 1 0 128 0z'
    internet<-cbind(internet,path)
    internet
    internet %>% 
      e_charts(kondisi) %>% 
      e_x_axis(splitLine=list(show = FALSE), 
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel= list(show=FALSE)) %>%
      e_y_axis(max=100, 
               splitLine=list(show = FALSE),
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel=list(show=FALSE)) %>%
      e_color(color = c('#B04759','#FFBABA')) %>%
      e_pictorial(persentase, symbol = path, z=10, name= 'realValue', 
                  symbolBoundingData= 100, symbolClip= TRUE) %>% 
      e_pictorial(persentase, symbol = path, name= 'background', 
                  symbolBoundingData= 100) %>% 
      e_labels(position = "bottom", offset= c(0, 8), 
               textStyle =list(fontSize= 30, fontFamily= 'Montserrat', 
                               fontWeight ='bold', 
                               color= '#B04759'),
               formatter="{@[1]}% ") %>%
      e_legend(show = FALSE) %>%
      e_title("                    Internet")
    
  })
  
  output$air<-renderEcharts4r({
    air<-air%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Tahun`==input$tahun2)%>%
      filter(`Jenjang`==input$jenjang2)
    persen<-round(air[,4]/air[,7]*100,2)
    kondisi<-"Cukup"
    colnames(persen)<-"persentase"
    air<-data.frame(air[,1:3],persen,kondisi)
    path ='path://M224 0c17.7 0 32 14.3 32 32V44l96-12c17.7 0 32 14.3 32 32s-14.3 32-32 32L256 84l-31-3.9-1-.1-1 .1L192 84 96 96C78.3 96 64 81.7 64 64s14.3-32 32-32l96 12V32c0-17.7 14.3-32 32-32zM0 224c0-17.7 14.3-32 32-32h96l22.6-22.6c6-6 14.1-9.4 22.6-9.4H192V116.2l32-4 32 4V160h18.7c8.5 0 16.6 3.4 22.6 9.4L320 192h32c88.4 0 160 71.6 160 160c0 17.7-14.3 32-32 32H416c-17.7 0-32-14.3-32-32s-14.3-32-32-32H315.9c-20.2 29-53.9 48-91.9 48s-71.7-19-91.9-48H32c-17.7 0-32-14.3-32-32V224zM436.8 423.4c1.9-4.5 6.3-7.4 11.2-7.4s9.2 2.9 11.2 7.4l18.2 42.4c1.8 4.1 2.7 8.6 2.7 13.1V480c0 17.7-14.3 32-32 32s-32-14.3-32-32v-1.2c0-4.5 .9-8.9 2.7-13.1l18.2-42.4z'
    air<-data.frame(air,path)
    air %>% 
      e_charts(kondisi) %>% 
      e_x_axis(splitLine=list(show = FALSE), 
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel= list(show=FALSE)) %>%
      e_y_axis(max=100, 
               splitLine=list(show = FALSE),
               axisTick=list(show=FALSE),
               axisLine=list(show=FALSE),
               axisLabel=list(show=FALSE)) %>%
      e_color(color = c('#0079FF','#ECF8F9')) %>%
      e_pictorial(persentase, symbol = path, z=10, name= 'realValue', 
                  symbolBoundingData= 100, symbolClip= TRUE) %>% 
      e_pictorial(persentase, symbol = path, name= 'background', 
                  symbolBoundingData= 100) %>% 
      e_labels(position = "bottom", offset= c(0, 8), 
               textStyle =list(fontSize= 30, fontFamily= 'Montserrat', 
                               fontWeight ='bold', 
                               color= '#0079FF'),
               formatter="{@[1]}% ") %>%
      e_legend(show = FALSE) %>%
      e_title("                        Air")
    
  })
  
  output$kelas<-renderPlot(({
    data<-kelas%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Jenjang`==input$jenjang2)%>%
      filter(`Tahun`==input$tahun2)
    baik<-round(data[,4]/data[,8],2)
    colnames(baik)="Kondisi"
    rr<-round(data[,5]/data[,8],2)
    colnames(rr)="Kondisi"
    rs<-round(data[,6]/data[,8],2)
    colnames(rs)="Kondisi"
    rb<-round(data[,7]/data[,8],2)
    colnames(rb)="Kondisi"
    dataa<-data.frame(c("Baik","Rusak Ringan","Rusak Sedang","Rusak Berat"),rbind(baik,rr,rs,rb))
    colnames(dataa)<-c("Kondisi","Persen")
    hsize<-2
    dataa<- dataa%>%mutate(x=hsize)
    font_family<-"Montserrat"
    label_pie<-percent(dataa$Persen)
    ggplot(dataa, aes(x = hsize, y = `Persen`, fill = `Kondisi`)) +
      ggtitle("Persentase Kondisi Kelas") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9")) +
      xlim(c(0.2, hsize + 0.5)) + theme_void() + 
      geom_label(aes(label = label_pie), position = position_stack(vjust = 0.5))+
      theme(legend.position = "right",
            legend.margin = margin(2,2,2,2),
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat")) 
    
  }))
  
  output$uks<-renderPlot(({
    data<-uks%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Jenjang`==input$jenjang2)%>%
      filter(`Tahun`==input$tahun2)
    baik<-round(data[,4]/data[,8],2)
    colnames(baik)="Kondisi"
    rr<-round(data[,5]/data[,8],2)
    colnames(rr)="Kondisi"
    rs<-round(data[,6]/data[,8],2)
    colnames(rs)="Kondisi"
    rb<-round(data[,7]/data[,8],2)
    colnames(rb)="Kondisi"
    dataa<-data.frame(c("Baik","Rusak Ringan","Rusak Sedang","Rusak Berat"),rbind(baik,rr,rs,rb))
    colnames(dataa)<-c("Kondisi","Persen")
    hsize<-2
    dataa<- dataa%>%mutate(x=hsize)
    font_family<-"Montserrat"
    label_pie<-percent(dataa$Persen)
    ggplot(dataa, aes(x = hsize, y = `Persen`, fill = `Kondisi`)) +
      ggtitle("Persentase Kondisi UKS") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9")) +
      xlim(c(0.2, hsize + 0.5)) + theme_void() +
      geom_label(aes(label = label_pie), position = position_stack(vjust = 0.5))+
      theme(legend.position = "left",
            legend.margin = margin(2,2,2,2),
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat"))
  }))
  
  output$lab<-renderPlot(({
    data<-lab%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Jenjang`==input$jenjang2)%>%
      filter(`Tahun`==input$tahun2)
    baik<-round(data[,4]/data[,8],2)
    colnames(baik)="Kondisi"
    rr<-round(data[,5]/data[,8],2)
    colnames(rr)="Kondisi"
    rs<-round(data[,6]/data[,8],2)
    colnames(rs)="Kondisi"
    rb<-round(data[,7]/data[,8],2)
    colnames(rb)="Kondisi"
    dataa<-data.frame(c("Baik","Rusak Ringan","Rusak Sedang","Rusak Berat"),rbind(baik,rr,rs,rb))
    colnames(dataa)<-c("Kondisi","Persen")
    hsize<-2
    dataa<- dataa%>%mutate(x=hsize)
    font_family<-"Montserrat"
    label_pie<-percent(dataa$Persen)
    ggplot(dataa, aes(x = hsize, y = `Persen`, fill = `Kondisi`)) +
      ggtitle("Persentase Kondisi Laboratorium") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9")) +
      xlim(c(0.2, hsize + 0.5)) + theme_void() +
      geom_label(aes(label = label_pie), position = position_stack(vjust = 0.5))+
      theme(legend.position = "right",
            legend.margin = margin(2,2,2,2),
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat")) 
  }))
  
  output$perpus<-renderPlot(({
    data<-perpus%>%
      filter(`Provinsi`==input$prov2)%>%
      filter(`Jenjang`==input$jenjang2)%>%
      filter(`Tahun`==input$tahun2)
    baik<-round(data[,4]/data[,8],2)
    colnames(baik)="Kondisi"
    rr<-round(data[,5]/data[,8],2)
    colnames(rr)="Kondisi"
    rs<-round(data[,6]/data[,8],2)
    colnames(rs)="Kondisi"
    rb<-round(data[,7]/data[,8],2)
    colnames(rb)="Kondisi"
    dataa<-data.frame(c("Baik","Rusak Ringan","Rusak Sedang","Rusak Berat"),rbind(baik,rr,rs,rb))
    colnames(dataa)<-c("Kondisi","Persen")
    hsize<-2
    dataa<- dataa%>%mutate(x=hsize)
    font_family<-"Montserrat"
    label_pie<-percent(dataa$Persen)
    ggplot(dataa, aes(x = hsize, y = `Persen`, fill = `Kondisi`)) +
      ggtitle("Persentase Kondisi Perpustakaan") +
      geom_col() +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#FF6F61", "#6B5B95", "#88B04B", "#F7CAC9")) +
      xlim(c(0.2, hsize + 0.5)) + theme_void() +
      geom_label(aes(label = label_pie), position = position_stack(vjust = 0.5))+
      theme(legend.position = "left",
            legend.margin = margin(2,2,2,2),
            plot.title = element_text(family = "Montserrat",face="bold",size = 18, hjust = 0.5),
            plot.subtitle = element_text(family = "Montserrat",face="plain",size = 12, hjust = 0.5),
            text = element_text(family = "Montserrat"))
  }))
  
  #USER BUAT KEMENDIKBUD
  output$kemendikbud <- renderImage({
    list(src="https://assets.pikiran-rakyat.com/crop/0x0:0x0/x/photo/2020/11/13/3486127848.jpg",height = 200, width = 200)
  },deleteFile = F)
  output$user <- renderUser({
    dashboardUser(
      name = "Kemendikbud Ristek",
      title = "Republik Indonesia",
      image = "https://th.bing.com/th/id/R.98d4506523b6d669691d2a5606a2c053?rik=WVR1xhtWwhTL0Q&riu=http%3a%2f%2fbiromahasiswa.helvetia.ac.id%2fwp-content%2fuploads%2fsites%2f13%2f2020%2f02%2flogo-kemdikbud.png&ehk=ZoMPgYqIHUKm%2byd7dIpJYOx6vy814tW9q0LSRcO%2bk9Y%3d&risl=&pid=ImgRaw&r=0", 
      subtitle = " ", 
      footer = p("Official link above", class = "text-center"),
      fluidRow(
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://data.kemdikbud.go.id/publikasi",
            icon = icon("book")
          )
        ),
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://repositori.kemdikbud.go.id/",
            icon = icon("table")
          )
        )
      )
    )
  })
  
  #DATA TABLE
  datasd <- database%>%filter(Jenjang=="SD")
  output$esde<- renderDT(datasd)
  datasmp <- database%>%filter(Jenjang=="SMP")
  output$esempe<- renderDT({datatable(datasmp)})
  datasma <- database%>%filter(Jenjang=="SMA")
  output$esema <- renderDT({datatable(datasma)})
  datasmk <- database%>%filter(Jenjang=="SMK")
  output$esemka <- renderDT({datatable(datasmk)})
  
  #FOTO
  output$adristy <- renderImage({
    list(src="www/Adristy.png",height=200,width=150)
  },deleteFile = F)
  output$aji <- renderImage({
    list(src="www/Aji.png",height=200,width=150)
  },deleteFile = F)
}

shinyApp(ui,server)