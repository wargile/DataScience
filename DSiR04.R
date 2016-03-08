
ff <- list.files("logs", full.names = TRUE)

ff <- list.files("logs", full.names = TRUE, 
                 pattern = "JRSPdata.*\\.log")

info <- file.info(ff)

summary(info$size/1024^2)


quantile(info$size/1024^2, seq(.9, 1, by = .01))


readLog <- 
function(filename = "logs/JRSPdata_2010_03_10_12_12_31.log",
         lines = readLines(filename))
{
  lines = grep("^#", lines, invert = TRUE, value = TRUE)
  els = strsplit(lines, "[[:space:]]+")

      # Get the interface and type so we can subset.
  iface = sapply(els, `[`, 4)
  type = sapply(els, `[`, 6)

      # find the indices corresponding to a position2d 
      # with a laser immediately after.
  i = which(iface == "position2d" & type == "001")
  i = i[ iface[i+1] == "laser" & type[i+1] == "001"]
    
      # Get the time, x, y, and then the range values 
      # from the laser below.
  locations = t(sapply(els[i], `[`, c(1, 8, 9)))
  ranges = t(sapply(els[i + 1], `[`, 
                               seq(14, by = 2, length = 361) ))

      # now combine these into a data frame
  locations = as.data.frame(lapply(1:ncol(locations),
                                    function(i) 
                                      as.numeric(locations[, i])))
  ranges = as.data.frame(lapply(1:ncol(ranges), 
                                 function(i) 
                                      as.numeric(ranges[, i]))) 
    
  ans = cbind(locations, ranges)
  names(ans) = c("time", "x", "y", 
                 sprintf("range%d", 1:ncol(ranges)))    

  invisible(ans)
}

cmd <- "grep position2d logs/JRSPdata_2010_03_10_12_12_31.log |
            grep -v ' 004 ' | cut -f 1,8,9 -d ' '"
txt <- system(cmd, intern = TRUE)

sh <- read.table(textConnection(txt))

cmd <- 
 "egrep 'laser|position2d' logs/JRSPdata_2010_03_10_12_12_31.log |
        grep -v ' 004 '| cut -f 4 -d ' '"
table(system(cmd, intern = TRUE))


sprintf("cut -d , -f %s",
         paste(seq(14, by = 2, length = 361), collapse = ","))

system.time(logs <- lapply(ff, readLog))
names(logs) <- ff

dur <- sapply(logs, function(x) x$time[nrow(x)] - x$time[1])

range(dur)


range(sapply(logs, function(ll) range(ll$x)))

range(sapply(logs, function(ll) range(ll$y)))


table(sapply(logs, function(ll) all( diff(ll$time) > 0 )))

deltas <- unlist(lapply(logs, function(ll) diff(ll$time)))
summary(deltas)


quantile(deltas, seq(.99, 1, length = 11))


which.max(deltas)


nrow(logs[["logs/JRSPdata_2010_03_10_12_39_46.log"]])


summary(sapply(logs, nrow))


ll <- logs[["logs/JRSPdata_2010_03_10_12_39_46.log"]]
i <- which.max(diff(ll$time))

delta.x <- unlist(lapply(logs, function(ll) diff(ll$x)))
delta.y <- unlist(lapply(logs, function(ll) diff(ll$y)))

velocity = 
  lapply(logs, function(ll)  
                 sqrt(diff(ll$x)^2 + diff(ll$y)^2)/diff(ll$time))

summary(unlist(lapply(logs, function(x) x[-(1:3)])))

makeColorRamp =
function(n)
{
  s = (1:n)/n
  zero = rep(0, n)
  rgb(s, (1-s), zero)
}

plot.RobotLog =
function(x, y, col = makeColorRamp(nrow(x)), ...)
{
   plot(y ~ x,  x, type = "p", pch = 20, col = col, ...)
   points(x$x[c(1, nrow(x))],  x$y[c(1, nrow(x))],
            pch = c("O", "+"), col = c("green", "blue"))
}

plotLook <-
function(row, ...)
{
  x = row[1, "x"]
  y = row[1, "y"]

  theta = seq(0, 2*pi, length = 360) - pi/2
  r = as.numeric(row[1, -c(1:3, 365)])
  x1 = x + r*cos(theta)
  y1 = y + r*sin(theta)
  par(pty = 's')
  plot(x + 2*cos(theta), y + 2*sin(theta), 
       col = "red", type = "l", 
       xlab = "x", ylab = "y", ...)    # 2 meter circle
  points(x1, y1, type = "l")           # what the robot sees
}

old = par(no.readonly = TRUE)
on.exit(par(old))
plotLook = plotLook2
e = unlist(lapply(logs, function(ll) ll$range1 - ll$range361))
summary(e)


ll$pos = sprintf("%.3f,%.3f", ll$x, ll$y)
w = duplicated(ll$pos)

if(any(w)) {
  tmp = ll[ ll$pos %in% ll$pos[w], ]
  errs = unlist(by(tmp[, 4:364], tmp$pos, scale, scale = FALSE,
                   simplify = FALSE))
}

getRangeErrors = 
function(ll)
{
   ll$pos = sprintf("%.3f,%.3f", ll$x, ll$y)
   w = duplicated(ll$pos)
   if(any(w)) {
      tmp = ll[ ll$pos %in% ll$pos[w], ]
      unlist(by(tmp[, 4:364], tmp$pos, scale, scale = FALSE,
                simplify = FALSE))
   } else
     numeric()
}

rangeErrs = unlist(lapply(logs, getRangeErrors))

finalLooks = lapply(logs, function(ll) ll[nrow(ll), ])

idx = c(19, 30, 4, 8,  2,  1, 78, 95, 48, 5, 39, 40) 
par(mfrow = c(4, 3), mar = rep(0, 4), pty = 's'); 
invisible(lapply(logs[idx], 
                  function(ll)
                     plotLook2(ll[ nrow(ll), ], axes = FALSE)))

rle(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))

getSegments =
    # return a list with elements being integer vectors
    # giving the indices of each contiguous segment
    # with values less than the threshold.
    #
    # We discard the 361st element.
function(range, threshold = 2)
{
  if(length(range) == 361)
     range = range[-361]
    
   rl = rle(range < threshold)

   cur = 1L
   ans = list()
   for(i in seq(along = rl$lengths)) {
      if(!rl$values[i]) {
         cur = cur + rl$lengths[i]
         next
      }
     ans[[length(ans) + 1L]] = seq(cur, length = rl$lengths[i])
     cur = cur + rl$lengths[i]
  }

  ans
}

length(getSegments(rep(2, 360)))

x = c(rep(2, 20), seq(1.7, 1.9, length = 10),
      rep(2, 50), seq(1.4, 1.6, length = 25),
      rep(2, 59), seq(.3, .5, length = 41))
x = c(x, rep(2, length = 361 - length(x)))

x = rep(2, 361)
x[21:30] = seq(1.7, 1.9, 10)
x[81:105] = seq(1.4, 1.6, 25)
x[165:205] = seq(.3, .5, 41)

getSegments(x)


sapply(getSegments(x, 1.6999), length)


getSegments(as.numeric(logs[[1]][1, -(1:3)]))

getSegments <-
    # return a list with elements being integer vectors
    # giving the indices of each contiguous segment
    # with values less than the threshold.
    #
    # We discard the 361st element.
function(range, threshold = 2)
{
  if(is.data.frame(range)) 
     range = as.numeric(range[1, -(1:3)])
    
  if(length(range) == 361)
     range = range[-361]
    
   rl = rle(range < threshold)

   cur = 1L
   ans = list()
   for(i in seq(along = rl$lengths)) {
      if(!rl$values[i]) {
         cur = cur + rl$lengths[i]
         next
      }
     ans[[length(ans) + 1L]] = seq(cur, length = rl$lengths[i])
     cur = cur + rl$lengths[i]
  }

  ans
}

getWrappedSegments =
function(range, threshold = 2,
         segments = getSegments(range, threshold))
{
   if(length(segments) > 1) {
      s1 = segments[[1]]
      s2 = segments[[length(segments)]]
      if(s1 == 1L && s2[length(s2)] == 360) {
         segments[[1]] = c(s2, s1)
         segments = segments[-length(segments)]
      }
   }
   
   segments
}

tmp = c(rep(1.5, 100), rep(2.0, 141), rep(1.5, 120))

getWrappedSegments(as.numeric(finalLooks[[1]][1, -(1:3)]))


getWrappedSegments =
function(range, threshold = 2,
         segments = getSegments(range, threshold),
         byDist = FALSE, ...)  # was byDist = TRUE
{
   force(byDist)
   if(is.data.frame(range)) {
       look = range
       range = as.numeric(range[ - c(1:3, 364)])
   } 
    
   if(length(segments) > 1) {
      s1 = segments[[1]]
      s2 = segments[[length(segments)]]
      if(s1 == 1L && s2[length(s2)] == 360) {
         segments[[1]] = c(s2, s1)
         segments = segments[-length(segments)]
      }
   }

   if(byDist)
     separateSegmentByDist(look, segs = segments, ...)
   else
     segments
}

separateSegmentByDist =
function(look, threshold = .15,
         segs = getWrappedSegments(as.numeric(look[ -(1:3) ])))
{
  r = as.numeric(look)[ -c(1:3, 364) ]
  theta = seq(0, 2*pi, length = 360)
  x = look$x + cos(theta)*r
  y = look$y + sin(theta)*r

  unlist(lapply(segs, separateSegment, x, y, threshold, r), recursive = FALSE)
}

circle.fit.nlm.funk <-
function (p, x, y) 
{
    x0 <- p[1]
    y0 <- p[2]
    r <- p[3]
    actual.r <- sqrt((x - x0)^2 + (y - y0)^2)
    sum((r - actual.r)^2)
}

look <- logs[[1]][nrow(logs[[1]]), ]

segs <- getWrappedSegments(as.numeric(look[1, -(1:3)]))

i = segs[[2]]
range = as.numeric(look[, -(1:3)])[i]
theta = seq(0, 2*pi, length = 360) - pi/2
xi = look$x + range * cos(theta[i])
yi = look$y + range * sin(theta[i])

plot(xi, yi, type = "l")

nlm(circle.fit.nlm.funk, c(x0 = mean(xi), y0 = mean(yi), r = .5),
     x = xi, y = yi)

circle.fit <-
function (x, y, initGuess = c(mean(x), mean(y), .5), ...) 
     nlm(circle.fit.nlm.funk, initGuess, x = x, y = y, ...)

robot.evaluation <-
function(look, min.length = 3, max.ss.ratio = 0.01, 
         min.radius = .5, max.radius = 2,
         range.threshold = 2,
         segs = getWrappedSegments(range, range.threshold),
         ...)
{
  x = look$x
  y = look$y
  range = as.numeric(look[, -(1:3)])
  theta = seq(0, 2*pi, length = 360)

  for(s in segs) {
     if(length(s) < min.length)
        next  

      xi = x + cos(theta[s]) * range[s]
      yi = y + sin(theta[s]) * range[s]
      out = circle.fit(xi, yi, ...)

      if(out$code > 3)  
         next

      if((out$minimum/length(s)) > max.ss.ratio)
         next

      if(abs(out$estimate[3]) < min.radius  
               || abs(out$estimate[3]) > max.radius) 
         next

      return(list(x = xi, y = yi, range = range[s], 
                  robot = c(x, y), fit = out))
  }
}

finalLooks = lapply(logs, function(x) x[nrow(x),])
circs = lapply(finalLooks, robot.evaluation)

unname(which(sapply(circs, length) > 0))

par(mfrow = c(7, 6), mar = rep(0, 4), pty = 's')
invisible(lapply(finalLooks[sapply(circs, length) > 0],
                     plotLook2, axes = FALSE))

i = which(sapply(circs, length) > 0)[c(5, 8, 14, 22, 37)]
unname(sapply(circs[i], function(o) c(o$fit$minimum, 
                                      o$fit$estimate[3],
                                      length(o$x))))


separateSegment =
function(idx, x, y, threshold = 0.15)
{
  xd = diff(x[idx])
  yd = diff(y[idx])
  d = sqrt(xd^2 + yd^2)
  if(any(d > threshold)) {
     i = which(d > threshold)[1]
     list(idx[1:(i-1)], idx[(i+1):length(idx)])
  } else
     list(idx)
}

circs = lapply(finalLooks, robot.evaluation)

hasCircle = as.logical(scan("logs/hasCircle100", 1L))

hasCircle.hat = sapply(circs, length) > 0
table(hasCircle, hasCircle.hat)


hasCircle[ is.na(hasCircle) ] = TRUE

plotLook3 =
function(row, threshold = 2, xlab = "x", ylab = "y", ...)
{
  x = row[1, "x"]
  y = row[1, "y"]

  theta = seq(0, 2*pi, length = 360) - pi/2
  r = as.numeric(row[1, -c(1:3, 364)])
  x1 = x + r*cos(theta)
  y1 = y + r*sin(theta)  
  segs = getWrappedSegments(r, threshold) 

  par(pty = 's')
  plot(x + 2*cos(theta), y + 2*sin(theta), col = "#33A02C", type = "l", xlab = xlab, ylab = ylab, ..., lty = 4)
  col = 1
  for(idx in segs) {
    points(x1[idx], y1[idx], type = "l", col = "#6A3D9A")
    col = col + 1
  }

  invisible(segs)
}

segs = lapply(finalLooks[missed], evalSegments, noCheck = TRUE)

segs[[7]][[1]]$fit


unname(sapply(segs[-7], 
        function(x) c(x[[1]]$fit$estimate[3], 
                      length(x[[1]]$x),
                      x[[1]]$fit$minimum/length(x[[1]]$range))))


missed = !hasCircle & hasCircle != hasCircle.hat
unname(lapply(circs[missed], `[[`, "fit"))

summary(sapply(circs[missed], function(x) x$fit$estimate[3]))


circs = lapply(finalLooks, robot.evaluation, 
                 max.radius = 0.9, min.radius = .475)
hasCircle.hat = sapply(circs, length) > 0
table(hasCircle, hasCircle.hat)


con = file("logs/JRSPdata_2010_03_10_12_12_31.log", "r")

while(length(line <- readLines(con, 1))) {
  if(grepl("^#", line))
     next

  vals = strsplit(line, "[[:space:]]+")[[1]]
  if(vals[6] != "001")  # the type field
     next

   # update look with values
   # call robot.evaluation to determine if we are 
   # looking at the target.
}
