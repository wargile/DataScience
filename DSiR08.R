
set.seed(7902423)
g = createGrid( numCars = .25)
g500 = runBML(g, 500)
g1000 = runBML(g500, 500)

pdf("FreeFlow_init.pdf", 5, 5); par(mar = rep(.3, 4)); plot(g); dev.off()
pdf("FreeFlow_500.pdf", 5, 5); par(mar = rep(.3, 4)); plot(g500); dev.off()
pdf("FreeFlow_1000.pdf", 5, 5); par(mar = rep(.3, 4)); plot(g1000); dev.off()

set.seed(10423)
gd = createGrid( numCars = .40)
gd500 = runBML(gd, 500)
gd1000 = runBML(gd500, 500)
#gd10000 = runBML(gd, 10000)

pdf("DeadlockGrid_init.pdf", 5, 5); par(mar = rep(.3, 4)); plot(gd); dev.off()
pdf("DeadlockGrid_500.pdf", 5, 5); par(mar = rep(.3, 4)); plot(gd500); dev.off()
pdf("DeadlockGrid_1000.pdf", 5, 5); par(mar = rep(.3, 4)); plot(gd1000); dev.off()

matrix(1:10, 5, 2)


createGrid = 
function(dims = c(100, 100), numCars = .3)
{
   if(length(dims) == 1)
     dims = rep(dims, 2)

   if(length(numCars) == 1 && numCars < 1) 
      numCars = rep(prod(dims) * numCars/2, 2)

   grid = matrix("", dims[1], dims[2])

   pos = sample(1:prod(dims), sum(numCars))
   grid[pos] = sample(rep(c("red", "blue"), numCars))

   grid
}
g = createGrid()
class(g)


dim(g)


table(g)


stopifnot(identical(table(g),
                    structure( c(7000L, 1500L, 1500L),
                               dim = 3L, class = "table",
                               dimnames = list(g = c("", 
                                                     "blue", 
                                                     "red")))))

createGrid(c(3, 5), .5)

createGrid(c(3, 5), .5)


createGrid = 
function(dims = c(100, 100), numCars = .3)
{
   if(length(dims) == 1)
     dims = rep(dims, 2)

   if(length(numCars) == 1 && numCars < 1)
      numCars = rep(prod(dims) * numCars/2, 2)

   grid = matrix("", dims[1], dims[2])

   pos = sample(1:prod(dims), sum(numCars))
   grid[pos] = sample(rep(c("red", "blue"), 
                          ceiling(numCars)))[seq(along = pos)]

   grid
}

set.seed(1456)
createGrid(c(3, 5), .5)


set.seed(1234)
createGrid(c(3, 5), c(9, 3))


set.seed(1234)
g = createGrid(c(3, 5), c(9, 3))
table(g)


print.BMLGrid =
function(x, ...)
    print(structure(x[nrow(x):1,], 
                    dimnames = list(nrow(x):1, 1:ncol(x))))

z = matrix(match(g, c("", "red", "blue")), nrow(g), ncol(g))

pdf("simpleImage.pdf", 5, 5)
par(mar = c(2.1, 3.1, .1, 1))
image(z, col = c("white", "red", "blue"))
dev.off()

plot.BMLGrid = 
function(x, ...)
{
   z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
   image(t(z), col = c("white", "red", "blue"),    
          axes = FALSE, xlab = "", ylab = "", ...)
   box()
}

plot.BMLGrid(g, main = "A sample title", sub = "A sub title")

set.seed(32351489)

pdf("grid100_2.pdf", 5, 5)
par(mar = rep(.1, 4))
plot.BMLGrid(g)
dev.off()

m = matrix("", 3, 4)
m[3, 1] = "blue"
m[1, 4] = "red"

m[row(m) == col(m) - 1] = "red"
m[col(m) == row(m) - 1] = "blue"

pdf("smallGrid.pdf", 5, 5)
par(mar = rep(.1, 4))
plot.BMLGrid(m)
dev.off()

class(g) = "BMLGrid"

summary(g)

class(g) = c("BMLGrid", "matrix")

class(g) = c("BMLGrid", class(g))

class(g) = c("SquareBMLGrid", "BMLGrid", "matrix")

class(g) = c("CoPrimeBMLGrid", "BMLGrid", "matrix")

body(createGrid)[[length(body(createGrid))]] = 
      quote(structure(grid, class = c("BMLGrid", "matrix")))

 class(g) = c("ExtendedBMLGrid", "BMLGrid", "matrix")

i = row(g)[g != ""]
j = col(g)[g != ""]
pos = cbind(i, j)

colors = g[pos]

cars = data.frame(i = i, j = j, colors = colors)

moveCars = 
function(grid, color = "red")
{
  i = row(grid)[grid != ""]
  j = col(grid)[grid != ""]
  pos = cbind(i, j)
  colors = grid[pos]
  cars = data.frame(i = i, j = j, colors = colors)

  w = which(cars$colors == color)
  for(idx in w) {

    curPos = c(i = cars$i[ idx ], j = cars$j[idx])
    nextPos = if(color == "red")
                c(curPos[1],
		  if(curPos[2] == ncol(grid))
                    1L
                  else
                    curPos[2] + 1L)
             else
                c(if(curPos[1] == nrow(grid))
                    1L
                  else
                    curPos[1] + 1L,
                  curPos[2])

        # check if nextPos is empty
    if(grid[ nextPos[1], nextPos[2] ] == "")  {
       grid[nextPos[1], nextPos[2]] = color
       grid[curPos[1], curPos[2]] = ""      
    }
   }

  grid
}
moveCars1 = moveCars
getCarLocations = 
function(g)
{
  i = row(g)[g != ""]
  j = col(g)[g != ""]
  pos = cbind(i, j)
  data.frame(i = i, j = j, colors = g[pos])
}
getCarLocations1 = getCarLocations
getNextPosition = 
function(curPos, dim, horizontal = TRUE) 
{
   if(horizontal) 
      c(curPos[1], 
        if(curPos[2] == dim[2]) 
           1L 
        else 
           curPos[2] + 1L)
   else 
       c(if(curPos[1] == dim[1]) 
            1L 
         else curPos[1] + 1L,
         curPos[2])
}
getNextPosition1 = getNextPosition
moveCars = 
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  for(idx in w) {
    curPos = c(cars$i[ idx ], cars$j[idx])
    nextPos = getNextPosition(curPos, dim(grid), color == "red")

        # check if nextPos is empty
    if(grid[ nextPos[1], nextPos[2] ] == "")  {
       grid[nextPos[1], nextPos[2]] = color
       grid[curPos[1], curPos[2]] = ""      
    }
   }

  grid
}
moveCars2 = moveCars
g = matrix("", 3,5)
g[cbind(c(1, 2, 2), c(1, 2, 4))] = "red"
g[cbind(c(2, 1, 1, 2), c(1, 3, 4, 5))] = "blue"
class(g) = c("BMLGrid", "matrix")

getCarLocations(g)


a = createGrid(c(4, 5), .7)
pos = getCarLocations(a)
nrow(pos) == sum(a != "")

getNextPosition(c(2, 3), dim = c(4, 5), horizontal = TRUE)

getNextPosition(c(2, 5), dim = c(4, 5), TRUE)

a = getNextPosition(c(2, 5), dim = c(4, 5), horizontal = FALSE)

getNextPosition(a, dim = c(4, 5), horizontal = TRUE)

g1 = moveCars(g)

g
g1

moveCars(g, "blue")


g2 = moveCars(moveCars(g), "blue")
g2


runBML = 
function(grid = createGrid(...), numSteps = 100, ...)
{
  for(i in 1:numSteps) {
    grid = moveCars(grid, "red")
    grid = moveCars(grid, "blue")
  }

  grid
}

g = createGrid()
g.out = runBML(g)

set.seed(1345)
g100 = createGrid(c(100, 100), .5)

tm1 = system.time(runBML(g100))

Rprof("/tmp/BML.prof")
g.out = runBML(g)
Rprof(NULL)
head(summaryRprof("/tmp/BML.prof")$by.self, 10)

profInfo = summaryRprof("/tmp/BML.prof")
moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  sz = dim(grid)
  horiz = (color == "red")
  for(idx in w) {
    curPos = c(cars$i[ idx ], cars$j[idx])
    nextPos = getNextPosition(curPos, sz, horiz)

        # check if nextPos is empty
    if(grid[nextPos[1], nextPos[2] ] == "")  {
       grid[nextPos[1], nextPos[2]] = color
       grid[curPos[1], curPos[2]] = ""      
    }
   }

  grid
}
moveCars3 = moveCars
tm2 = system.time(runBML(g100))
tm1/tm2


gs = matrix("", 4, 7)
gs[cbind( c(1, 2, 2, 4, 4, 4, 4), c(1, 4, 7, 1, 2, 3, 5)) ] = "red"
gs[cbind( c(1, 1, 2, 2, 2, 2, 4), c(5, 7, 2, 3, 5, 6, 6))] = "blue"
class(gs) = c("BMLGrid", "matrix" )

pos = getCarLocations(gs)
red = pos$colors == "red"
rows = pos$i[red]
cols = pos$j[red]

nextRows = rows
nextCols = ifelse(cols == ncol(gs), 1L, cols + 1L)

cbind(cols, nextCols)


gs[ cbind(nextRows, nextCols) ] 


w = gs[ cbind(nextRows, nextCols) ]  == ""

cbind(rows, cols)[w,]


cbind(nextRows, nextCols)[w,]


gs[ cbind(nextRows, nextCols)[w,] ] = "red"
gs[ cbind(rows, cols)[w,] ] = ""

moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  rows = cars$i[w]
  cols = cars$j[w]
 
  if(color == "red") {
    nextRows = rows
    nextCols = ifelse(cols == ncol(grid), 1L, cols + 1L)
  } else {
    nextRows = ifelse(rows == nrow(grid), 1L, rows + 1L)
    nextCols = cols
  }

  w = grid[ cbind(nextRows, nextCols) ]  == ""
  grid[ cbind(nextRows, nextCols)[w, , drop = FALSE] ] = color
  grid[ cbind(rows, cols)[w,, drop = FALSE] ] = ""

  grid
}
moveCars4 = moveCars
tm_v = system.time(runBML(g100))
tm2/tm_v


Rprof("/tmp/BML.prof")
g.out = runBML(g)
Rprof(NULL)
head(summaryRprof("/tmp/BML.prof")$by.self, 10)


nextCols = cols + 1L
nextCols[ nextCols > ncol(grid) ] = 1L

moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  rows = cars$i[w]
  cols = cars$j[w]
 
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[ nextCols > ncol(grid) ]  = 1L
  } else {
    nextRows = rows + 1L
    nextRows[ nextRows > nrow(grid) ] = 1L
    nextCols = cols
  }

  w = grid[ cbind(nextRows, nextCols) ]  == ""
  grid[ cbind(nextRows, nextCols)[w,, drop = FALSE] ] = color
  grid[ cbind(rows, cols)[w, , drop = FALSE] ] = ""

  grid
}
moveCars5 = moveCars
tm_v2 = system.time(runBML(g100))
tm2/tm_v2


getCarLocations =
function(g)
{
  i = row(g)[g != ""]
  j = col(g)[g != ""]
  pos = cbind(i, j)
  structure(pos, dimnames = list(g[pos], c("i", "j")))
}
getCarLocations2 = getCarLocations
moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(rownames(cars) == color)
  rows = cars[w, 1]
  cols = cars[w, 2]
 
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[ nextCols > ncol(grid) ]  = 1L
  } else {
    nextRows = rows + 1L
    nextRows[ nextRows > nrow(grid) ] = 1L
    nextCols = cols
  }

  w = grid[ cbind(nextRows, nextCols) ]  == ""
  grid[ cbind(nextRows, nextCols)[w, , drop = FALSE] ] = color
  grid[ cbind(rows, cols)[w, , drop = FALSE] ] = ""

  grid
}
moveCars6 = moveCars
tm_v3 = system.time(runBML(g100))
tm_v2/tm_v3


getCarLocations =
function(g)
{
  w = (g != "")
  i = row(g)[w]
  j = col(g)[w]
  pos = cbind(i, j)
  structure(pos, dimnames = list(g[pos], c("i", "j")))
}
getCarLocations2 = getCarLocations
moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(rownames(cars) == color)
  rows = cars[w, 1]
  cols = cars[w, 2]
 
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[ nextCols > ncol(grid) ]  = 1L 
  } else {
    nextRows = rows + 1L
    nextRows[ nextRows > nrow(grid) ] = 1L
    nextCols = cols
  }

  nextLocs = cbind(nextRows, nextCols)
  w = grid[ nextLocs ]  == ""
  grid[ nextLocs[w, , drop = FALSE] ] = color
  grid[ cbind(rows, cols)[w,, drop = FALSE] ] = ""

  grid
}
moveCars7 = moveCars
tm_v4 = system.time(runBML(g100))
tm_v3/tm_v4


N = 2^(3:20) 
timings = 
   sapply(N, 
          function(n) {
            print(n)
            g = createGrid(as.integer(rep(sqrt(n), 2)), .5)
            system.time(runBML(g))
          })
plot(N, timings[3,], type = "p", 
       xlab = "Number of grid cells", 
       ylab = "Elapsed Time (seconds)")
abline(lm(timings[3,] ~ N))

dyn.load("BML.so")

stopifnot(is.loaded("R_BML_simultaneous"))

g = createGrid(c(1000, 1000), .5)
gi = matrix(match(g, c("red", "blue"), 0L), nrow(g), ncol(g))

pos = getCarLocations(g)
red = pos[ rownames(pos) == "red", ]
blue = pos[ rownames(pos) == "blue", ]

val = .C("R_BML_simultaneous", 
         grid = gi, matrix(0, nrow(gi), ncol(gi)), dim(gi),
         red, nrow(red), blue, nrow(blue), 
         1000L, FALSE, velocity = integer(2*1000))
g1 = val$grid
class(g1) = c("BMLGrid", "matrix")

plot.BMLGrid =
function(x, xlab = "", ylab = "", ...)
{
   if(typeof(x) == "character")
     z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
   else
     z = x
   image(t(z), col = c("white", "red", "blue"), 
          axes = FALSE, xlab = xlab, ylab = ylab, ...)
   box()
}

crunBML =
function(grid, numIter = 100L, check = FALSE)
{
    k = class(grid)
    gi = gridToIntegerGrid(grid)

    velocity = matrix(0L, as.integer(numIter), 2L,
                       dimnames = list(NULL, c("red", "blue")))
    pos = getCarLocations(gi)
    red = pos[rownames(pos) == "1",]
    blue = pos[rownames(pos) == "2",]    
    ans = .C("R_BML_simultaneous", gi, grid = gi, dim(gi), 
                                   red = red, nrow(red), 
                                   blue = blue, nrow(blue), 
                                   as.integer(numIter), FALSE,
                                   velocity = velocity)

    ans = ans[c("grid", "velocity")]
    class(ans$grid) = k
    ans
}

gridToIntegerGrid = 
function(g)
  gi = matrix(match(g, c("red", "blue"), 0L), nrow(g), ncol(g))

tm_c = system.time(o <- crunBML(g100))
tm_v4/tm_c


timings = rbind(loop1 = tm1, loop2 = tm2, 
                vector1 = tm_v, vector2 = tm_v2, 
                vector3 = tm_v3, vector4 = tm_v4, 
                C = tm_c)[,1:3]


runGrid = 
function(dims, numCars, numIter = 1000, plot = TRUE)
{
  grid = createGrid(dims, numCars)
  g.out = crunBML(grid, numIter)
  if(plot) {
    plot(grid)
    plot(g.out$grid)
  }

  invisible(list(initial = grid, 
                 final = g.out$grid, 
                 velocity = g.out$velocity))
}

par(mfrow = c(1, 3))
z = runGrid(c(1000, 1000), .35, plot = TRUE)
plot(rowSums(z$velocity), type = "l",
      main = "Number of cars moving in each pair of time steps")

set.seed(13123)

runs = lapply(c(.25, .33, .38, .38, .55, .65),
               function(density)
                 runGrid(1024, density, 30000, FALSE))

set.seed(43234)

run = runGrid(20, .1)

colMeans(run$velocity)


tmp = colMeans(run$velocity)
tmp/table(run$final)[c("1", "2")]

