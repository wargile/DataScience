
files = read.csv("Data/baseballArchiveFileNames.txt",
                 stringsAsFactors = FALSE, header = FALSE)[[1]]
dfNames = gsub(".csv", "", files)
fullFileNames = paste("Data/", files, sep = "")
names(fullFileNames) = dfNames

##Problem with the BatingPost table
fullFileNames = fullFileNames[ dfNames != "BattingPost"]
dfNames = dfNames[ dfNames != "BattingPost"]


library("RSQLite")
drv = dbDriver("SQLite")
tfile = tempfile()
con=dbConnect(drv, dbname = tfile)

sapply(dfNames, function(df) {
                   x = read.csv(fullFileNames[df], sep = ",",  header = TRUE,
                                   stringsAsFactors = FALSE)
                   dbWriteTable(con, df, x)
})

dbListTables(con)

dbListFields(con, "Salaries")

query = "SELECT COUNT(*) FROM Salaries;"
dbGetQuery(con, query)

salaryDF = dbGetQuery(con, "SELECT * FROM Salaries;")

query = "SELECT salary, teamID, yearID FROM Salaries;"
salaryDF = dbGetQuery(con, query)

class(salaryDF)

dim(salaryDF)

sapply(salaryDF, class) 

payrolls = tapply(salaryDF$salary, salaryDF[ -1], sum)

dbGetQuery(con, "SELECT MIN(yearID) from Salaries;")
dbGetQuery(con, "SELECT MAX(yearID) from Salaries;")

dbGetQuery(con, "SELECT MIN(yearID), MAX(yearID) from Salaries;")

query = "SELECT DISTINCT yearID from Salaries;"
years = dbGetQuery(con, query)

length(years[[1]])

dbGetQuery(con, "SELECT MIN(yearID), MAX(yearID) from Teams;")

query = "SELECT  * FROM Salaries LIMIT 6;"
dbGetQuery(con, query)

inflation = c(1,    1.02, 1.06, 1.10, 1.15, 1.21, 
              1.27, 1.30, 1.34, 1.38, 1.42, 1.46, 1.49, 1.51, 1.55, 1.60,
              1.65, 1.67, 1.71, 1.76, 1.82, 1.87, 1.93, 2.00, 1.99, 2.03,
              2.09, 2.13)

names(inflation) = 1985:2012

payrolls = dbGetQuery(con,
                   "SELECT yearID AS year, teamID AS team,
                          lgID AS league, SUM(salary) AS payroll
                    FROM Salaries GROUP BY team, year;")


head(inflation)

payrollStd = mapply(function(pay, year)
                          pay/inflation[as.character(year)],
                    payrolls$payroll, payrolls$year)

payrolls$payStd = payrollStd

pdf(file = "PayrollPlotBW.pdf", width = 10, height = 5)
oldPar = par(mar = c(4.1, 4.1, 0.5, 0.5))

payrolls$payrollStd = payrollStd/1000000

boxplot(payrollStd ~ league + year, data = payrolls, log = "y", 
        ylim = c(5,125), col = gray.colors(2), axes = FALSE,
        ylab = "Inflation-adjusted Payroll (millions)" )

axis(1, at = seq(1.5, 55, by = 10), 
     labels = seq(1985, 2010, by = 5))
axis(2)
legend("topleft", 
       legend= c("American League", "National League"),
       fill = gray.colors(2), bty = "n")

par(oldPar)
dev.off()

dbListFields(con, "SeriesPost")

dbListFields(con, "Teams")

query = 
  "SELECT Salaries.yearID AS year, Teams.name AS team, 
        Salaries.teamID AS id, SUM(Salaries.salary) AS payroll
     FROM Salaries, Teams
     WHERE Salaries.teamID = Teams.teamID AND
           Salaries.yearID = Teams.yearID
     GROUP BY Salaries.teamID, Salaries.yearID;"

payrollWN = dbGetQuery(con, query)

all(payrollWN$payroll == payrolls$payroll)

dim(payrollWN)

dim(payrolls)

table(payrolls$team)

table(payrollWN$id)

all(payrolls$payroll[payrolls$team != "KC"] == payrollWN$payroll)

queryFcn = function() {
  TeamInfo = dbGetQuery(con, 
                       "SELECT yearID, teamID, name FROM Teams;")
  SalaryInfo = dbGetQuery(con,  
                  "SELECT yearID, teamID, salary FROM Salaries;")
  b = merge(TeamInfo, SalaryInfo, by = c("yearID", "teamID"),  
            all.y = TRUE)
  payroll = tapply(b$salary, b[ , (1:2)], sum, simplify = FALSE )
}

system.time(replicate(100, invisible(dbGetQuery(con, query))))

system.time(replicate(100, invisible(queryFcn())))

dbGetQuery(con, "SELECT round, teamIDwinner, teamIDloser 
                 FROM SeriesPost WHERE yearID = '2012';")

query =
  "SELECT Salaries.yearID AS year, Salaries.teamID AS team,  
      SUM(Salaries.salary) AS payroll, SeriesPost.round as round
    FROM Salaries LEFT OUTER JOIN SeriesPost 
    ON SeriesPost.teamIDwinner = Salaries.teamID
       AND SeriesPost.yearID = Salaries.yearID
       AND SeriesPost.round = 'WS'
    GROUP BY team, year;"

payrolls = dbGetQuery(con, query)

payrollStd = mapply(function(pay, year) {
  pay/inflation[as.character(year)] },
  payrolls$payroll, payrolls$year)

payrolls$payrollStd = payrollStd/1000000

pdf(file = "PayrollPlotWWS.pdf", width = 10, height = 5)
oldPar = par(mar = c(4.1, 4.1, 0.5, 0.5))

ptCol = rgb(190, 190, 190, 150, max = 256)
with(payrolls, plot(payrollStd ~ jitter(year, 0.5), 
                    ylab = "Payroll (inflation-adjusted millions)",
                    ylim = c(5,125), log = "y",
                    xlab = "Year",
                    pch = 19, cex = 0.8, col = ptCol))
with(payrolls[payrolls$round == 'WS',],
     points(y = payrollStd , x = year, pch = 19, col = "red", cex = 0.8) )
with(payrolls[payrolls$round == 'WS',],
     text(y = payrollStd , x = year, labels = team, pos = 3, cex = 0.8) )

par(oldPar)
dev.off()

sals = dbReadTable(con, "Salaries")
sort( unique( sals$salary[sals$yearID == 2003] ), 
      decreasing = TRUE)[1:3]

query = "SELECT DISTINCT Salary FROM Salaries 
           WHERE yearID = 2003 ORDER BY Salary DESC
           LIMIT 3;"

query = "SELECT DISTINCT Salary FROM Salaries 
           WHERE yearID = 2003 ORDER BY Salary DESC;"
res = dbSendQuery(con, query)

topSalary = fetch(res, n = 3) 
topSalary

fetch(res, n = 4)

dbClearResult(res)

query1 = "SELECT COUNT(*) FROM Salaries WHERE yearID = 2003;"
totCount = dbGetQuery( con, query1)

query2 = "SELECT Salary FROM Salaries WHERE yearID = 2003;"
res = dbSendQuery( con, query2)

blockSize = 200
totRead = 0
top3Salary = NULL

while (totRead < totCount) {
  top3Salary = sort(unique( c(top3Salary, 
                              fetch( res, n = blockSize)[[1]]) ),
                    decreasing = TRUE )[1:3]
  totRead = totRead + blockSize
}

top3Salary

dbClearResult(res)

charSalary = paste(top3Salary, collapse = ", ")
query3 = paste("SELECT playerID FROM Salaries WHERE yearID = 2003
                AND Salary IN (", charSalary, ") ;", sep = "")
dbGetQuery(con, query3)


dbDisconnect(con) 
dbUnloadDriver(drv)
