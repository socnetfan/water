
library(qdapRegex)
library(foreign)

rm(list=ls())

zip <- read.dbf("tl_2019_us_zcta510.dbf", as.is = FALSE)[,1]

list <- data.frame(zip=integer(),
                   ncont=character(),
                   chromium=character(),
                   chromiumx=character(),
                   nitrate=character(),
                   nitratex=character(),
                   radium=character(),
                   radiumx=character(),
                   TTHMs=character(),
                   TTHMsx=character(),
                   stringsAsFactors=FALSE)

for (z in 1:nrow(zip)) {
  addr <- paste("https://www.ewg.org/tapwater/search-results.php?zip5=",zip[z,1],"&searchtype=zip", sep="")
  thepage = readLines(addr)
  o <- grep('VIEW UTILITY',thepage)
  p2 <- c1 <- c2 <- n1 <- n2 <- r1 <- r2 <- t1 <- t2 <- "0"
  if (length(o)>0) {
    p <- c(rm_between(thepage[o], "system.php?pws=", "\">VIEW UTILITY</a>", extract=TRUE))
    addr2 <- paste("https://www.ewg.org/tapwater/system.php?pws=",p, sep="")
    thepage2 <- readLines(addr2)
    o2 <- grep('<p>EXCEED<br>EWG HEALTH<br>GUIDELINES</p>',thepage2)
    if (length(o2)>0) {
      p2 <- c(rm_between(thepage2[o2-1], "<p class=\"contaminant-tile-number\">", "</p>", extract=TRUE))
      c <- grep('Chromium \\(hexavalent\\) was',thepage2)
      if (length(c)>0) {
        c2 <- c(rm_between(thepage2[c], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
        c1 <- c(rm_between(thepage2[c+5], "<span>", " ppb</span>", extract=TRUE))
      }
      n <- grep('Nitrate was found',thepage2)
      if (length(n)>0) {
        n2 <- c(rm_between(thepage2[n], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
        n1 <- c(rm_between(thepage2[n+5], "<span>", " ppm</span>", extract=TRUE))
      }
      r <- grep('Radium, combined \\(-226 \\& -228\\) was found',thepage2)
      if (length(r)>0) {
        r2 <- c(rm_between(thepage2[r], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
        r1 <- c(rm_between(thepage2[r+5], "<span>", " pCi/L</span>", extract=TRUE))
      }
      t <- grep('Total trihalomethanes \\(TTHMs\\) was found',thepage2)
      if (length(t)>0) {
        t2 <- c(rm_between(thepage2[t], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
        t1 <- c(rm_between(thepage2[t+5], "<span>", " ppb</span>", extract=TRUE))
      }
    }
  } else {
    h <- grep('Utility name',thepage)
    if (length(h)>0) {
      if (nchar(thepage[h[1]+5])>20) {
        h <- h[1]
      } else {
        h <- h[1]+1
      }
      i <- c(rm_between(thepage[h+5], 'a href=\"', '\">', extract=TRUE))[1]
      j <- paste("https://www.ewg.org/tapwater/",i,sep="")
      thepage2 = readLines(j)
      o2 <- grep('<p>EXCEED<br>EWG HEALTH<br>GUIDELINES</p>',thepage2)
      if (length(o2)>0) {
        p2 <- c(rm_between(thepage2[o2-1], "<p class=\"contaminant-tile-number\">", "</p>", extract=TRUE))
        c <- grep('Chromium \\(hexavalent\\) was',thepage2)
        if (length(c)>0) {
          c2 <- c(rm_between(thepage2[c], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
          c1 <- c(rm_between(thepage2[c+5], "<span>", " ppb</span>", extract=TRUE))
        }
        n <- grep('Nitrate was found',thepage2)
        if (length(n)>0) {
          n2 <- c(rm_between(thepage2[n], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
          n1 <- c(rm_between(thepage2[n+5], "<span>", " ppm</span>", extract=TRUE))
        }
        r <- grep('Radium, combined \\(-226 \\& -228\\) was found',thepage2)
        if (length(r)>0) {
          r2 <- c(rm_between(thepage2[r], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
          r1 <- c(rm_between(thepage2[r+5], "<span>", " pCi/L</span>", extract=TRUE))
        }
        t <- grep('Total trihalomethanes \\(TTHMs\\) was found',thepage2)
        if (length(t)>0) {
          t2 <- c(rm_between(thepage2[t], "was found at ", "times above EWG's Health Guideline.", extract=TRUE))
          t1 <- c(rm_between(thepage2[t+5], "<span>", " ppb</span>", extract=TRUE))
        }
      }
    }
  }
  q <- data.frame(cbind(zip[z,1],p2,c1,c2,n1,n2,r1,r2,t1,t2))
  names(q) <- c("zip","ncont","chromium","chromiumx","nitrate","nitratex","radium","radiumx","TTHMs","TTHMsx")
  list <- rbind(list,q)
  print(z)
}
write.csv(list, file = "ewg_water.csv")



















