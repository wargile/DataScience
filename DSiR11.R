
urlLatLon = "http://dev.maxmind.com/static/csv/codes/country_latlon.csv"

load("Data/CIAFactbookDFs.rda")

urlLatLon = paste("http://dev.maxmind.com/static/csv/",
                  "codes/country_latlon.csv", sep = "")

latlonDF = read.csv(urlLatLon)

head(latlonDF)

head(popDF)

head(infMortDF)

nrow(latlonDF)

IMPop = merge(infMortDF, popDF, by = "ctry", all = FALSE)
dim(IMPop)

latlonDF$code = tolower(as.character(latlonDF$iso.3166.country))

allCtryData = merge(IMPop, latlonDF, by.x = "ctry", by.y = "code",
                    all = FALSE)

library(RColorBrewer)
display.brewer.all()
cols = brewer.pal(9, "YlOrRd")[c(1, 2, 4, 6, 7)]

newInfMort = cut(allCtryData$infMort, breaks = 5)
summary(newInfMort)

newInfMort2 = cut(allCtryData$infMort, 
                  breaks = c(0, 37, 50, 65, 80, 150))
summary(newInfMort2)

pdf(file = "CIA_HistogramIM.pdf", width = 5, height = 3.5)

hist(allCtryData$infMort, breaks = 20, main = "",
     xlab = "Infant Mortality per 1000 Live Births")

dev.off()

options(digits = 2)

quantile(allCtryData$infMort, probs = seq(0, 1, by = 0.2))      

InfMortDiscrete = cut(allCtryData$infMort, 
                      breaks = c(0, 10, 25, 50, 75, 150))

summary(InfMortDiscrete)

pdf("CIA_MapMixUp.pdf", width = 8, height = 5.4)

library(maps)
world = map(database = "world", fill = TRUE, col="light grey")
 
symbols(allCtryData$longitude, allCtryData$latitude, add = TRUE,
        circles= sqrt(allCtryData$pop)/4000, inches = FALSE,
        fg = cols[InfMortDiscrete], bg = cols[InfMortDiscrete])

legend(x = -150, y = 0, title = "Infant Mortality",
       legend = levels(InfMortDiscrete), fill = cols, cex = 0.8)

dev.off()

range(allCtryData$pop)

pdf("CIA_HistogramPop.pdf", width = 8, height = 5.4)

hist(sqrt(allCtryData$pop), breaks = 20, 
     xlab = "Square-root of Population", main = "")

dev.off()

rads = pmax(sqrt(allCtryData$pop)/4000, 1)

pdf("temp.pdf", width = 7, height = 4)
world = map(database = "world", fill = TRUE, col="light grey")
symbols(allCtryData$longitude, allCtryData$latitude, 
        circles= rads, add = TRUE, inches = FALSE, 
        fg = cols[InfMortDiscrete],
        bg = cols[InfMortDiscrete])
legend(x = -150, y = 0, title = "Infant Mortality",
       legend = levels(InfMortDiscrete), fill = cols, cex = 0.8)
dev.off()

allCtryData[ allCtryData$ctry %in% c("ch","gb"), ]

head(codeMapDF)

IMPopBothCodes = merge(IMPop, codeMapDF, by.x = "ctry", 
                       by.y = "cia", all = FALSE)
allCtryData = merge(IMPopBothCodes, latlonDF, by.x = "iso", 
                    by.y = "iso.3166.country", all = FALSE)

head(allCtryData)

allCtryData[allCtryData$ctry %in% c("ch", "sz", "gb", "uk"), ]

rads= pmax(sqrt(allCtryData$pop)/4000, 1)
InfMortDiscrete = cut(allCtryData$infMort, 
                      breaks = c(0, 10, 25, 50, 75, 150))

pdf("CIA_MapFixed.pdf", width = 8, height = 5.4)
world = map(database = "world", fill = TRUE, col="light grey")
symbols(allCtryData$longitude, allCtryData$latitude, 
        circles= rads, 
        add = TRUE, inches = FALSE, 
        fg = cols[InfMortDiscrete],
        bg = cols[InfMortDiscrete])
legend(x = -150, y = 0, title = "Infant Mortality",
       legend = levels(InfMortDiscrete), fill = cols, cex = 0.8)
dev.off()
 
library(RKML)
doc = kmlPoints(allCtryData)

saveXML(doc, "countryPlain.kml")

rads = sqrt(allCtryData$pop)/4000
summary(rads)

popBreaks = c(0, 0.5, 1, 2, 4, 10)
popDiscrete = cut(rads, breaks = popBreaks)
table(popDiscrete)

popScales =  as.character(1+ c(0.25, .75, 1.5, 3, 7))
icon = rep(sprintf("yor%dball.png", 
                   seq(along = levels(InfMortDiscrete))),
           each = length(levels(popDiscrete)))
scale = rep(popScales, length(levels(InfMortDiscrete)))

ballStyles = mapply(function(scale, icon)
                      list(IconStyle = 
                             list(scale = scale,
                                  Icon = c(href = icon))),
                    scale, icon, SIMPLIFY = FALSE)

g = expand.grid(seq(along = levels(InfMortDiscrete)),
                seq(along = levels(popDiscrete)))
names(ballStyles) = sprintf("ball_%d-%d", g[,2], g[,1])

ctryStyle = sprintf("ball_%d-%d", InfMortDiscrete, popDiscrete)

ptDescriptions = 
 sprintf(paste(
   "<table><tr><td>Country:</td><td>%s</td></tr>",
   "<tr><td>Infant Mortality:</td>",
   "<td>%s per 1,000 live births</td></tr>",
   "<tr><td>Population:</td><td>%s</td></tr></table>"),
         allCtryData$name, allCtryData$infMort, allCtryData$pop)

docName = "Infant Mortality"
docDescription = "2012 CIA Factbook"
folderName = "Countries"

doc = kmlPoints(allCtryData, docName = docName, 
                docDescription = docDescription,  
                docStyles = ballStyles, 
                folderName = folderName,
                style = ctryStyle, 
                description = ptDescriptions,
                ids = allCtryData$ctry,
                .names = allCtryData$name)
 
kmlLegend(x = 20, y = 20, title = "Infant Mortality",
          legend = levels(InfMortDiscrete), fill = cols,
          text.col = "white", dims = c(100, 108),
          parent = doc)

saveXML(doc, "ctryFancy.kml")

library(XML)
factbookDoc = xmlParse("Data/factbook.xml.gz")

factbookRoot = xmlRoot(factbookDoc)
xmlName(factbookRoot)

xmlSize(factbookRoot)

table(names(factbookRoot))

sapply(factbookRoot["category"], function(node) table(names(node)))

sapply(factbookRoot["category"], xmlAttrs)

categoryNodes = factbookRoot["category"]
w = sapply(categoryNodes, xmlGetAttr, "name")=="People and Society"

Ids = sapply(categoryNodes[[ which(w) ]] [ "field" ], 
               xmlGetAttr, "id")

f2091Index = which(Ids == "f2091")
f2091Index

rankNodes = 
  categoryNodes[[ which(w) ]][ "field" ][[ f2091Index ]]["rank"]
xmlSize(rankNodes)

infMortNum = sapply(rankNodes, xmlGetAttr, "number")
infMortCtry = sapply(rankNodes, xmlGetAttr, "country")

head(infMortNum)

head(infMortCtry)

field2091 = getNodeSet(factbookDoc, "//field[@id='f2091']")

xmlAttrs(field2091[[1]])

rankNodes = getNodeSet(factbookDoc, "//field[@id='f2091']/rank")

xmlAttrs(rankNodes[[1]])

infNum = as.numeric(sapply(rankNodes, xmlGetAttr, "number"))
infCtry = sapply(rankNodes, xmlGetAttr, "country")

infMortDF = data.frame(infMort = infNum, ctry = infCtry,
                       stringsAsFactors = FALSE)               

rankNodes = getNodeSet(factbookRoot, "//field[@id='f2119']/rank")
popNum = as.numeric(sapply(rankNodes, xmlGetAttr, "number"))
popCtry = sapply(rankNodes, xmlGetAttr, "country")

popDF = data.frame(pop = popNum, ctry = popCtry, 
                   stringsAsFactors = FALSE)

rows = getNodeSet(factbookRoot, 
                  "//table[ columnHeader[@title='ISO 3166']]/row")

codeMap = sapply(rows, function(row) {
 cia = xmlGetAttr(row[[1]], "country");
 name = xmlGetAttr(row[[1]], "content");
 iso  = xmlGetAttr(row[[3]], "content");
c(cia, name, iso) })

codeMapDF = as.data.frame(t(codeMap), stringsAsFactors = FALSE)
names(codeMapDF) = c("cia", "name", "iso")

IMPopBothCodes = merge(IMPop, codeMapDF, by.x = "ctry", 
                       by.y = "cia", all = FALSE)
allCtryData = merge(IMPopBothCodes, latlonDF, by.x = "iso", 
                    by.y = "iso.3166.country", all = FALSE)

allCtryData[allCtryData$ctry %in% c("ch", "sz", "gb", "uk"), ]

doc = newXMLDoc()

aRoot = newXMLNode("a", doc = doc)

newXMLNode("b", parent = aRoot)

cNode = newXMLNode("c", "Some Text", newXMLNode("d"),
                   parent = aRoot)

newXMLNode("e", attrs = c(id = "eId"), parent = cNode)

saveXML(doc, "sample.xml")

makeBaseDocument = 
  function(docName = "Infant mortality", 
           docDesc =  "2012 CIA Factbook", 
           lat = 43, lon = -121, alt = 4100000, 
           tilt = 0, heading = 0)
{ 
  doc = newXMLDoc()
  rootNode = newXMLNode("kml", doc = doc)
  DocNode = newXMLNode("Document", parent = rootNode)
  newXMLNode("name", docName , parent = DocNode)
  newXMLNode("description", docDesc, parent = DocNode)
  LANode = newXMLNode("LookAt", parent = DocNode)
  newXMLNode("longitude", lon, parent = LANode)
  newXMLNode("latitude", lat, parent = LANode)
  newXMLNode("altitude", alt, parent = LANode)
  newXMLNode("tilt", tilt, parent = LANode)
  newXMLNode("heading", heading, parent = LANode)
  newXMLNode("altitudeMode", "absolute", parent = LANode)
  newXMLNode("Folder", parent = DocNode)
  return(doc)
}

baseDoc = makeBaseDocument()
baseDoc

addPlacemark = function(lat, lon, id, label, parent){
  newXMLNode("Placemark", 
             newXMLNode("name", label),
             newXMLNode("Point", 
                        newXMLNode("coordinates", 
                                paste(lon, lat, 0, sep = ","))),
             attrs = c(id = id), parent = parent)
}

root = xmlRoot(baseDoc)
folder = root[["Document"]][["Folder"]]

mapply(addPlacemark,
       lat = allCtryData$latitude, lon = allCtryData$longitude,
       id = allCtryData$ctry, label = allCtryData$name,
       parent = folder)

addPlacemark = 
 function(lat, lon, id, label, parent, style = NULL, desc = NULL)
{
  pm = newXMLNode("Placemark", newXMLNode("name", label),
                  newXMLNode("Point", 
                             newXMLNode("coordinates", 
                                    paste(lon, lat, 0, sep =","))),
                  attrs = c(id = id), parent = parent)
  if (!is.null(style)) newXMLNode("styleUrl", style, parent = pm)
  if (!is.null(desc)) newXMLNode("description", desc, parent = pm)    
}

mapply(addPlacemark,
       lat = allCtryData$latitude, lon = allCtryData$longitude,
       id = allCtryData$ctry, label = allCtryData$name,   
       parent = folder, style = ctryStyle, desc = ptDescriptions)

makeStyleNode = function(styleInfo, id){
  st = newXMLNode("Style", attrs = c("id" = id))
  newXMLNode("IconStyle", 
             newXMLNode("scale", styleInfo$IconStyle$scale), 
             newXMLNode("Icon", styleInfo$IconStyle$Icon),
             parent = st)
  return(st)
}

styleNodes = mapply(makeStyleNode, ballStyles, names(ballStyles))

addChildren(root[["Document"]], kids = styleNodes, at = 3)

saveXML(baseDoc, file = "countryMashup.kml")

addPlacemarks.fast =
function(lon, lat, parent)
{
  txt = sprintf("<Placemark><Point><coordinates>%.3f,%.3f,0
                  </coordinates></Point></Placemark>",
                lon, lat)
  parseXMLAndAdd( paste(txt, collapse = ""), parent)
}

makePM = function(x, y, parent) { 
  newXMLNode("Placemark", 
             newXMLNode("Point", 
                        newXMLNode("coordinates", 
                                   paste(x, y, 0, sep=","))),
             parent = parent)
}

addPlacemarks.slow =
function(lon, lat, parent)
{
  mapply(makePM, x = lon, y = lat, parent = parent) 
}

doc = newXMLDoc()
root = newXMLNode("kml", doc = doc)
folder = newXMLNode("Folder", parent = root)

lons = rep(allCtryData$longitude, 10)
lats = rep(allCtryData$latitude, 10)

system.time(invisible(
              addPlacemarks.slow(lons, lats, folder)))

rm(doc)
doc = newXMLDoc()
root = newXMLNode("kml", doc = doc)
folder = newXMLNode("Folder", parent = root)

system.time(invisible(
              addPlacemarks.fast(lons, lats, folder)))

urlLatLonHTML = "http://dev.maxmind.com/geoip/legacy/codes/country_latlon/"
