library("shiny")
library("scoring")


## Define vector to hold forecasts + scores
fcasts <- NULL
items <- NULL
cond <- NULL
scores <- NULL
i <- 0  # For stimuli
flatamt <- 10  ## Flat payment for simply participating
txt <- ""  ## Outcome text
sliderValues <- NULL
psv <- 0

## Scoring table
params <<- rbind(c(1,1), c(3,5)) # scoring rule parameters
load('sctable.rda')
load('fstable.rda')
## CODE TO GET SCTABLE:
## sctable <- matrix(NA, 4, 11)
## p <- seq(0, 1, .1)
## sctable[1,] <- calcscore(p, rep(0,length(p)), params[1,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## sctable[2,] <- calcscore(p, rep(1,length(p)), params[1,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## sctable[3,] <- calcscore(p, rep(0,length(p)), params[2,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## sctable[4,] <- calcscore(p, rep(1,length(p)), params[2,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## sctable <- round(sctable)
## #sctable[2,8] <- 45; sctable[2,4] <- 25
## sctable <- rbind(p, sctable) #apply(sctable,2,function(x) paste(x,"\uA2",sep="")))
## sctable <- cbind(c("Your forecast:","If the hurricane misses Miami, you get:",
##                    "If the hurricane hits Miami, you get:",
##                    "If the hurricane misses Miami, you get:",
##                    "If the hurricane hits Miami, you get:"), sctable)
## rownames(sctable) <- NULL
## CODE TO GET FULL SCORING TABLE:
## fstable <- matrix(NA, 4, 101)
## p <- seq(0, 1, .01)
## fstable[1,] <- calcscore(p, rep(0,length(p)), params[1,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## fstable[2,] <- calcscore(p, rep(1,length(p)), params[1,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## fstable[3,] <- calcscore(p, rep(0,length(p)), params[2,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## fstable[4,] <- calcscore(p, rep(1,length(p)), params[2,], bounds=c(0,1000), fam="beta", reverse=TRUE)
## fstable <- round(fstable,2)

## Load outcomes
load("stimuli/houtcomes.rda")


shinyServer(function(input, output, session) {

  ## TODO Change number of trials
  ## Randomize trials
  nprac <- 3
  nstim <- 3
  ## Randomize condition
  j <- sample(1:2, 1)
  ## Ensure one hurricane and (nstim - 1) others
  ## (19 and 21 are out)
  out.trials <- c(11,15,19,21,23)
  prac.trials <- sample(c(27,5,23,20,18,7,30,6,3,12), nprac)
  ## Experimental trials:
  trnum <- sample(c(11,14,9,22,17,25,16,8,28,13,2,4,10,24,26), nstim)
  ## Issue: When stimuli are repeated, they don't show up 2nd time.
  ## Solution: Duplicate files saved in gimp (hurricanes 31-60)
  ##   This is not as simple as renaming the files; the system
  ##   seems to recognize identical files of different names.
  trnum2 <- sample(trnum, nstim)
  hc <- outcomes[trnum, 1]
  hc2 <- outcomes[trnum2, 1]
  slowst <- outcomes[trnum, 2]
  slowst2 <- outcomes[trnum2, 2]
  ## Practice trials:
  pracnum <- sample(prac.trials, 3)
  prachc <- outcomes[pracnum, 1]
  pracslowst <- outcomes[pracnum, 2]
  
  ## Figure out how many have already done the study, assign a unique sub number
  res.files <- system("ls data/demo*.csv", intern = TRUE)
  if(length(res.files) > 0){
    res.files <- strsplit(res.files,"o")
    snums <- unlist(lapply(res.files, function(x) strsplit(x[2],"[.]")[[1]][1]))
    maxsnum <- max(as.numeric(snums))
    this.sub <- maxsnum + 1
  } else {
    this.sub <- 1
  }
  demoname <- paste("data/demo",this.sub,".csv",sep="")
  outname <- paste("data/sub",this.sub,".csv",sep="")
  ## "Claim" this subject number
  system(paste("touch", outname))
  system(paste("touch", demoname))
  
  output$p1text <- renderText({
    "<em>Throughout the study, please do not use the back or refresh buttons on your browser!</em><h3>Consent To Participate in Research</h3><p>Insert consent form here.</p><br><p><em>Throughout the study, please do not use the back or refresh buttons on your browser!</em></p>"
  })

  ## Demographic text
  output$demtext <- renderText({
    "<h3>First, please tell us a little about yourself.</h3><br>"
  })
  
  ## Instructions p 1
  output$text1 <- renderText({
    if(input$d1 == 1){
      demores <- c(input$education, as.numeric(input$age))
      write.csv(demores, file = demoname, row.names=FALSE)
    }      
    "<p> In this study, you will assume the role of a hurricane forecaster in Miami, Florida.</p><br><p>You will view information about tropical storms in the Atlantic, then forecast the probability that a hurricane will make a direct hit on Miami.</p><br><br>"
  })

  ## Instructions p 2
  output$text2 <- renderText({
      updateSliderInput(session, "age", value=18)
    "<p>To help you make forecasts, you will receive three pieces of information about a tropical storm.  These pieces of information can influence your forecast, as described below:</p><br><p>1. The storm's current location, presented on a map: as a storm gets closer to Miami, you have a better idea about whether it will hit Miami.</p><br><p>2. The storm's intensity (wind speed): To be considered a hurricane, the wind speed must be 74mph or greater at landfall.  If the storm hits Miami with lower wind speeds, it is not considered a hurricane.</p><br><p>3. The storm's trajectory (direction in which the storm is currently headed): Storms typically move west-northwest, then gradually curve until their movement is changed to north-east.</p><br><br>"
  })

  ## Instructions p 3
  output$text3 <- renderText({
    '<p>You will report your forecast on a probability scale from 0 to 1, where</p><br><p>0.0 means "a hurricane will definitely not strike Miami".</p><p>0.1 means "there is a 10% chance a hurricane will strike Miami."</p><p>0.2 means "there is a 20% chance a hurricane will strike Miami."</p><p>0.3 means "there is a 30% chance a hurricane will strike Miami."</p><p>0.4 means "there is a 40% chance a hurricane will strike Miami."</p><p>0.5 means "there is a 50% chance a hurricane will strike Miami."</p><p>0.6 means "there is a 60% chance a hurricane will strike Miami."</p><p>0.7 means "there is a 70% chance a hurricane will strike Miami."</p><p>0.8 means "there is an 80% chance a hurricane will strike Miami."</p><p>0.9 means "there is a 90% chance a hurricane will strike Miami."</p><p>1.0 means "a hurricane will definitely strike Miami".</p><br><br>'
    })

  ## Instructions p 4
  output$text4 <- renderText({
    if(input$p1==0) return()
    "<p>You will receive XX dollars simply for your participation in this study.  In addition, you will have the opportunity to receive a bonus for being a good forecaster.  Your bonus payment will be based on the points that you accumulate.</p><br><p>On each trial, you can accumulate a maximum of 1,000 points.  These points are converted to dollars at the rate of XX points per dollar.  The point systems are set up so that accurate forecasts make a larger amount of money than inaccurate forecasts, and the point systems will be described as the study progresses.</p><br><p>You will accumulate points on every trial (excluding the practice trials), and we will determine your payment using the points.</p><br><br>"
  })

  ## Instructions p 5
  output$text5 <- renderText({
    if(input$p1==0) return()
    "<p>Importantly, throughout the entire study, you can always maximize your expected bonus through honesty: if you believe that the probability of a hurricane is .05 (say), then it is always in your best interest to report a forecast of .05.</p><br><p>You will now make 3 practice forecasts.</p><br><br>"
  })

  ## Scoring text 1
  output$scoretext <- renderText({
    if(input$p1==0) return()
    "<p>For the following set of forecasts, you will receive points that are related to your forecast accuracy.  The (rounded) point system is described below (forecasts not shown receive an amount in between the adjacent forecasts that are displayed).</p><br><p>You will be reminded of this point system as you make each forecast.  Importantly, you can maximize your expected points by reporting your honest beliefs.</p><br>"
  })

  ## Scoring text 2
  output$scoretext2 <- renderText({
    if(input$p1==0) return()
    "<p>For the following set of forecasts, you will receive points that are related to your forecast accuracy.  The (rounded) point system is described below (forecasts not shown receive an amount in between the adjacent forecasts that are displayed).</p><br><p>You will be reminded of this point system as you make each forecast.  Importantly, you can maximize your expected points by reporting your honest beliefs.</p><br>"
  })

  ## Scoring tables
  output$scoring1 <- renderTable({
    if(input$p1==0) return()
    tmptab <- sctable[c(1,2*j,(2*j+1)),]

    tmptab}, include.rownames=FALSE, include.colnames=FALSE)

  output$scoring2 <- renderTable({
    if(input$p1==0) return()
    tmptab <- sctable[c(1,(2*(3-j)),(2*(3-j)+1)),]

    tmptab}, include.rownames=FALSE, include.colnames=FALSE)


  
  ## Practice plots
  output$prac1 <- renderImage({
    if(input$p1==0){
      list(src = 'loading.gif')
    } else {
    k <- input$okp + 1
    fname <- paste("stimuli/hforecast",pracnum[k],".png",sep="")
    psv <- isolate(as.numeric(input$slprac))

    if(input$instruct==5){
      if(input$submitp > input$okp & input$sc1 == 0){
        ##fcasts <- c(fcasts, psv)
        ##items <- c(items, pracnum[k])
        ##cond <- c(cond, 0)
        ##print(cbind(cond, items, fcasts))
        write.table(matrix(c(0, pracnum[k], psv), 1, 3), file = outname,
                    row.names=FALSE, append=TRUE, col.names=FALSE, sep=",")
      }
    }
    
    updateSliderInput(session, "slprac", value=1/2)

    list(src = fname)}
    }, deleteFile = FALSE)

  ## Practice results
  output$prac2 <- renderImage({
    if(input$p1==0){
      list(src = 'loading.gif')
    } else {
    k <- input$okp + 1
    fname <- paste("stimuli/hurricane",pracnum[k],".gif",sep="")

    list(src = fname)}
    }, deleteFile = FALSE)

  output$pracres <- renderText({
    if(input$p1==0) return()
    k <- input$okp
    if(pracslowst[k]==1){
      txt <- "The storm hit Miami, but the wind speeds were not high enough to be a hurricane."
    } else if(prachc[k]==1){
      txt <- "The hurricane hit Miami!"
    } else {
      txt <- "The storm missed Miami."
    }
    paste("<p><br><br>",txt,"<br><br></p>", sep="")
  })

  ## Output scores as slider moves
  output$c1slider <- renderTable({
    data.frame(
      Name = c("Your points if the hurricane misses:",
        "Your points if the hurricane hits:"),
      Value = as.character(c(fstable[(j*2-1),1+100*input$c1sl], fstable[j*2,1+100*input$c1sl]))
      )
  }, include.rownames=FALSE, include.colnames=FALSE)
  
  ## NB For three-page sequences, k must use the button from page 2.
  ## Condition 1 plots
  output$c1 <- renderImage({
    if(input$p1==0){
      list(src = 'loading.gif')
    } else {
    k <- input$ok + 1
    fname <- paste("stimuli/hforecast",trnum[k],".png",sep="")
    psv <- isolate(as.numeric(input$c1sl))

    ## TODO: After add scoring instructions/training, probably
    ##       need input$instruct > 5 here
    if(input$instruct == 5){
      if(input$submit > input$ok){
        ##fcasts <- c(fcasts, psv)
        ##items <- c(items, trnum[k])
        ##cond <- c(cond, j)
        ##print(cbind(cond, items, fcasts))
        write.table(matrix(c(j, trnum[k], psv), 1, 3), file = outname,
                    row.names=FALSE, append=TRUE, col.names=FALSE, sep=",")
      }
    }

    updateSliderInput(session, "c1sl", value=1/2)

    list(src = fname)}}, deleteFile = FALSE)

  ## Condition 1 results
  output$c1res <- renderImage({
    if(input$p1==0){
      list(src = 'loading.gif')
    } else {
      k <- input$ok + 1
    fname <- paste("stimuli/hurricane",trnum[k],".gif",sep="")
    ##print(fname)
    list(src = fname)}}, deleteFile = FALSE)

  output$c1score <- renderText({
    i.old <- input$submit
    k <- j
    out <- hc
    ss <- slowst

    if(i.old > 0){
      if(ss[i.old]==1){
        txt <- "The storm hit Miami, but the wind speeds were not high enough to be a hurricane."
      } else if(i.old > 0 & out[i.old]==1){
        txt <- "The hurricane hit Miami!"
      } else {
        txt <- "The storm missed Miami."
      }

      tmp <- read.table(outname, header=FALSE, sep=",")
      scores <- tmp[nrow(tmp),4]
    } else {
      scores <- 0
    }

    paste("<p><br><br>",txt,"<br><br> You received ",scores," points from this forecast.<br><br></p>", sep="")
  })

  ## TODO: We still need this, but the logical conditions need
  ##       to be changed because each person only sees one condition.
  ## Condition 2 plots
  output$c2 <- renderImage({
    if(input$p1==0){
      list(src = 'loading.gif')
    } else {
    k <- input$okc2 + 1
    fname <- paste("stimuli/hforecast",trnum2[k],".png",sep="")
    psv <- isolate(as.numeric(input$c2sl))

    if(input$instruct == 5){
      if(input$submitc2 > input$okc2){
        ##fcasts <- c(fcasts, psv)
        ##items <- c(items, trnum2[k])
        ##cond <- c(cond, (3 - j))
        ##print(cbind(cond, items, fcasts))
        ##write.csv(cbind(cond, items, fcasts), file = outname, row.names=FALSE)
        write.table(matrix(c(3-j, trnum2[k], psv), 1, 3), file = outname,
                    row.names=FALSE, append=TRUE, col.names=FALSE, sep=",")
      }
    }

    updateSliderInput(session, "c2sl", value=1/2)

    list(src = fname)}}, deleteFile = FALSE)

  ## Output scores as slider moves
  output$c2slider <- renderTable({
    data.frame(
      Name = c("Your points if the hurricane misses:",
        "Your points if the hurricane hits:"),
      Value = as.character(c(fstable[((3-j)*2-1),1+100*input$c2sl],
        fstable[(3-j)*2,1+100*input$c2sl]))
      )
  }, include.rownames=FALSE, include.colnames=FALSE)
  
  ## Condition 2 results
  output$c2res <- renderImage({
    if(input$p1==0){
      list(src = 'loading.gif')
    } else {
    k <- input$okc2 + 1
    fname <- paste("stimuli/hurricane",trnum2[k]+30,".gif",sep="")
    ##print(fname)
    list(src = fname)}}, deleteFile = FALSE)

  ## Condition 2 score
  output$c2score <- renderText({
    if(input$p1==0) return()

    i.old <- input$submit
    k <- 3 - j
    out <- hc2
    ss <- slowst2
    
    if(ss[i.old]==1){
      txt <- "The storm hit Miami, but the wind speeds were not high enough to be a hurricane."
    } else if(out[i.old]==1){
      txt <- "The hurricane hit Miami!"
    } else {
      txt <- "The storm missed Miami."
    }

    tmp <- read.table(outname, header=FALSE, sep=",")
    scores <- tmp[nrow(tmp),4]
    paste("<p><br><br>",txt,"<br><br> You received ",scores," points from this forecast.<br><br></p>", sep="")
  })
  
  
  ## Scoring
  observe({
    if(input$p1==0) return()
    i <- i.old <- input$submit
    k <- j
    out <- hc2
    ss <- slowst2
    if(k == 2){
      out <- hc
      ss <- slowst
    }

    ## Reset if we are restarting the study via back button or page reloading
    if(input$instruct == 0) scores <- NULL
    
    if(input$sc1 == 1 & input$okp == nprac){
      ## TODO This logical condition needs to be modified
      if(input$ok > input$ok2 &
         (input$instruct != 7 | input$ok >= nstim)){
        tmp <- read.table(outname, header=FALSE, sep=",", fill=TRUE)
        sv <- tmp[nrow(tmp), 3]

        tmp[nrow(tmp), 4] <- fstable[(2*k - (1 - out[i.old])),(1+100*sv)]
        write.table(tmp, file=outname, row.names=FALSE, col.names=FALSE,
                    sep=",")
                     #round(calcscore(out[i.old] ~ sv, fam = "beta",
                     #                        param = params[k,],
                     #                        bounds = c(0,.5), reverse = TRUE), 2))

      }
    }
  })
  
  
  output$endtext <- renderText({
    if(input$p1==0) return()
    ## TODO If ntrials!=15, modify
    if(input$ok2 == nstim){
      ## Write completion code + amount earned
      tmp <- read.table(outname, header=FALSE, sep=",")
      tmp <- tmp[(nprac+1):(nprac + nstim),4]
      sscore <- sum(tmp)/2000
      write.table(cbind(this.sub, 1112*this.sub, round(sscore + flatamt,2)),
                  "data/subpayments.dat",
                  sep=",",
                  append=TRUE, row.names=FALSE, col.names=FALSE)

    }
    paste("<p>Thank you for participating.  Based on the flat payment plus the bonus based on your forecasts, you earned $",formatC(flatamt + sscore,2,format='f'),".</p><p>Please enter completion code ",1112*this.sub," on Mechanical Turk.</p>",sep="")
  })

  output$loading <- renderImage({
    list(src="loading.gif")}, deleteFile=FALSE)
  
})
