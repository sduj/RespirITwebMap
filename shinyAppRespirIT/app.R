###############################################################################################################
# PROJECT: RespirIT
# SCRIPT: Renders 1ha resolution raster maps of 3 allergenic tree species across Belgium using R Shiny 
# BY: Sébastien DUJARDIN & Corentin VISEE
# DATE: 31/05/2021
# EMAIL: sebastien.dujardin@unamur.be
# WEBMAP LINK: https://s-dujardin.shinyapps.io/shinyapprespirit/
# REFERENCE: https://researchportal.unamur.be/en/publications/mapping-allergenic-tree-species-in-belgium-a-webmap-application
# GitHub: https://github.com/sduj/RespirITwebMap/
# MAP DETAILS: https://doi.org/10.1016/j.landurbplan.2021.104286
###############################################################################################################

# Load packages
library('raster') # get raster data
library('dplyr') # for pipes %>%
library('leaflet') # for interactive maps
library('viridis') # nice color
library('shiny') # to deploy online --> see rsconnect --> set up once
library('rgdal') # necessary for raster creation
library('shinydashboard') # nice layout for shiny apps
library('stringr') # to get first word from radiobuttons choice

### Get data + create palette -------------------------------------------------

rangePalette = c(-1, 55)
# here i get all tif files that are in my working directory but i don't load them
# i pipe values & range (the result of one operation is used by the next line)
# it's needed because the free subscription at shinyapps.io gives only 1gb of ram
# if we load all the raster data --> exceed that 1gb limits

paletteCustom = colorNumeric(
    palette = c("#0C2C84", "#41B6C4", "#FFFFCC"),
    domain = rangePalette,
    na.color = "transparent"
)
# this is a custom palette
# args palette need to be specify in hex color
# i give 3 color --> 1 for lowest, 1 for mid & 1 for highest
# continuous scale is done automatically

mapLeafletTemplate = leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.WorldImagery")%>%
    addLegend(pal = paletteCustom,
              values = rangePalette
              , title = "% cover") %>%
    addMiniMap(toggleDisplay = T) %>%
    addScaleBar(position = 'bottomleft')

# this is a template for our leaflet maps
# legend + minimap + scalebar on it


### Get ready our shiny app part 1 --> first UserInterface = ui ---------------

# Set up dashboard page for ui
# cut our screen in a header (title), siderbar for choices & a body for our map
# see shinydashboard for other nice layout https://rstudio.github.io/shinydashboard/
# radiobuttons = multiple choice with one selected by default
# download button thanks to :
# https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
# text ouput linked to txt file in main directory (see server interface)

ui = dashboardPage(
    header = dashboardHeader(title = 'RespirIT'),
    sidebar = dashboardSidebar(
        radioButtons(
            inputId = 'TypeArbre',
            label = "Allergenic tree species",
            choices = c("Alnus spp.", "Betula spp.", "Corylus avellana"),
            selected = "Alnus spp."
        ),
        downloadButton(outputId = "download", label = "Download map"),
        textOutput(outputId = "authorInformation")
    ),
    body = dashboardBody(
                    leafletOutput(
                        outputId = "MapLeaflet",
                        height = 650, 
                        width = 'auto'
                    )
    )
)


## Get ready our shiny app part 2 ---------------------------------------------

server = function(input, output, session) {
    typeArbre = reactive(x = {
        input$TypeArbre
    })
    # get our input tree from the radiobuttons ui
    
    # Render Map leaflet
    # here i use multiple if statement --> maybe not the fastest / cleanest way
    # see if there is a way to avoid complete loading of our template & raster:
    # template could stay
    # all if = get leaflet template and add raster on it
    # again, i don't store the raster --> memory limitation
    output$MapLeaflet = renderLeaflet(expr = {
        if (typeArbre() == 'Alnus spp.') {
            # Import to leaflet
            
            mapLeafletTemplate %>%
                addRasterImage(
                    x = raster(x = 'DataProj/Alnus.tif'),
                    project = F,
                    maxBytes = 6.5 * 10 ^ 6,
                    colors = paletteCustom,
                    opacity = 0.8
                )
        }
        else if (typeArbre() == 'Betula spp.') {
            # Import to leaflet
            mapLeafletTemplate %>%
                addRasterImage(
                    x = raster(x = 'DataProj/Betula.tif'),
                    project = F,
                    colors = paletteCustom,
                    maxBytes = 6.5 * 10 ^ 6,
                    opacity = 0.8
                )
        }
        else if (typeArbre() == 'Corylus avellana') {
            # Import to leaflet
            mapLeafletTemplate %>%
                addRasterImage(
                    x = raster(x = 'DataProj/Corylus.tif'),
                    project = F,
                    colors = paletteCustom,
                    maxBytes = 6.5 * 10 ^ 6,
                    opacity = 0.8
                )
        }
        else{
            leaflet() %>%
                addTiles()
        }
    })
    
    # Link to our donwload button  (see link for help L61)
    output$download = downloadHandler(
        filename = function() {
            paste0(
                word(string = input$TypeArbre, start = 1), 
                '.zip'
            ) # zip file name = treeChosen.zip
            # word func needed to get only species (problem with spp. --> spp..zip !)
            },
        content = function(file) {
            # first step is to store data paths
            fs = c()
            pathData = paste0(
                word(string = input$TypeArbre, start = 1), 
                '.tif')
            fs = c(fs, pathData)
            # second step is to write data
            writeRaster(
                x = raster(x = paste0(
                    'DataProj/', 
                    word(string = input$TypeArbre, start = 1), 
                    '.tif'
                )),
                filename = pathData,
                overwrite = T
            )
            
            pathData = 'infoMap.txt'
            fs = c(fs, pathData)
            writeLines(text = readLines(con = 'infoMap.txt', warn = T))
            # use writeLine for small txt file
            # see data.table::fread and data.table::fwrite for big files
            
            # third and last step --> zip our files
            zip(zipfile = file, files = fs)
            
        }
    )
    
    # get our txt file with information and render it
    output$authorInformation = renderText(expr = {
        infoMap = readLines(con = 'infoMap.txt', warn = T)
    })
}

# Load the app --------------------------------------------------------------------
shinyApp(ui = ui, server = server)
