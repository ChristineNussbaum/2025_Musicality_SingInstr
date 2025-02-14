#TracedEZOut-Function
# by Dr. Verena Skuk and Christine Nussbaum. 09.2020

library(schoRsch)

#this Script is a minimal adjustment of the anova_out function from the SchoRsch-Package
# F- and np2-Values are printed with 3 digits behind the comma
# changes are marked with CN

#######################################################################################
#######################################################################################
#######################################################################################

tracedEzOut <-  function(ezout, print = TRUE, sph.cor = "GG", mau.p = 0.05, 
                        etasq = "partial", dfsep = ", ", corr.df = FALSE, show.eps = 0) 
{
  x <- ezout
  if (toupper(sph.cor) != "GG" & toupper(sph.cor) != "HF" & 
      toupper(sph.cor) != "NO") {
    sph.cor = "no"
    print(paste("Warning: Unknown correction method specified!", 
                " Reporting uncorrected p-values instead.", sep = ""), 
          quote = FALSE)
  }
  if (etasq != "partial" & etasq != "generalized") {
    etasq = "partial"
    print(paste("Warning: Unknown effect size specified!", 
                " Reporting partial eta squared instead.", sep = ""), 
          quote = FALSE)
  }
  if (show.eps != 0 & show.eps != 1 & show.eps != 2 & show.eps != 
      3) {
    show.eps = 0
    print(paste("Warning: Unknown reporting method for epsilon specified!", 
                " Omitting epsilon statistics for violations of sphericity.", 
                sep = ""), quote = FALSE)
  }
  doeswork <- 1
  if ("ANOVA" %in% names(x)) {  #CN
    if ("SSn" %in% colnames(x$ANOVA) && "SSd" %in% colnames(x$ANOVA)) {
      outtable <- data.frame(Effect = x$ANOVA$Effect, MSE = x$ANOVA$SSd/x$ANOVA$DFd, 
                             df1 = x$ANOVA$DFn, df2 = x$ANOVA$DFd, F = format(round(x$ANOVA$F, 
                                                                                    3), nsmall = 2), p = format(round(x$ANOVA$p, 
                                                                                                                      3), nsmall = 3), petasq = format(round(x$ANOVA$SSn/(x$ANOVA$SSn + 
                                                                                                                                                                            x$ANOVA$SSd), 3), nsmall = 2), getasq = format(round(x$ANOVA$ges, 
                                                                                                                                                                                                                                 3), nsmall = 2))
    }
    else {
      outtable <- "Couldn't find Sum of Squares in ezANOVA output. Please use the 'detailed=TRUE' option!"
      doeswork <- 0
    }
  }
  else {
    outtable <- "N/A ... possibly wrong input?"
    doeswork <- 0
  }
  if (doeswork < 1) {
    print(outtable)
  }
  else {
    if ("Mauchly's Test for Sphericity" %in% names(x)) {
      outspher <- data.frame(Effect = x$"Mauchly's Test for Sphericity"$Effect, 
                             p_Mauchly = format(round(x$"Mauchly's Test for Sphericity"$p, 
                                                      3), nsmall = 3), GGEpsilon = format(round(x$"Sphericity Corrections"$GGe, 
                                                                                                3), nsmall = 3), p_GG = format(round(x$"Sphericity Corrections"$"p[GG]", 
                                                                                                                                     3), nsmall = 3), HFEpsilon = format(round(x$"Sphericity Corrections"$HFe, 
                                                                                                                                                                               3), nsmall = 3), p_HF = format(round(x$"Sphericity Corrections"$"p[HF]", 
                                                                                                                                                                                                                    3), nsmall = 3))
    }
    else {
      outspher <- "N/A"
      sph.cor = "no"
    }
    if ("ANOVA" %in% names(x)) {
      txttable <- outtable
      txttable$epsilon <- ""
      ajdffcts <- list()
      if (toupper(sph.cor) != "NO") {
        for (isph in 1:length(x$"Sphericity Corrections"$Effect)) {
          if (x$"Mauchly's Test for Sphericity"$p[isph] <= 
              mau.p) {
            eoi <- x$"Sphericity Corrections"$Effect[isph]
            for (iaov in 1:length(x$ANOVA$Effect)) {
              if (x$ANOVA[iaov, 1] == eoi) {
                if (toupper(sph.cor) == "GG") {
                  pmaucorr <- format(round(x$"Sphericity Corrections"$"p[GG]"[isph], 
                                           3), nsmall = 3)
                  if (R.version$major < 4) {
                    levels(txttable$p) <- c(levels(txttable$p), 
                                            pmaucorr)
                  }
                  txttable[iaov, 6] <- pmaucorr
                  if (corr.df == TRUE) {
                    corr.df1 <- format(round(x$"Sphericity Corrections"$GGe[isph] * 
                                               as.numeric(txttable[iaov, 3]), 
                                             2), nsmall = 2)
                    corr.df2 <- format(round(x$"Sphericity Corrections"$GGe[isph] * 
                                               as.numeric(txttable[iaov, 4]), 
                                             2), nsmall = 2)
                    txttable[iaov, 3] = corr.df1
                    if (show.eps == 1) {
                      txttable[iaov, 4] <- paste(corr.df2, 
                                                 dfsep, "e = ", format(round(x$"Sphericity Corrections"$GGe[isph], 
                                                                             2), nsmall = 2), sep = "")
                    }
                    else {
                      txttable[iaov, 4] <- corr.df2
                    }
                  }
                  else if (show.eps == 1) {
                    txttable[iaov, 4] <- paste(txttable[iaov, 
                                                        4], dfsep, "e = ", format(round(x$"Sphericity Corrections"$GGe[isph], 
                                                                                        2), nsmall = 2), sep = "")
                  }
                  else if (show.eps == 2) {
                    txttable$epsilon[iaov] <- paste(" (e = ", 
                                                    format(round(x$"Sphericity Corrections"$GGe[isph], 
                                                                 2), nsmall = 2), ")", sep = "")
                  }
                  if (show.eps >= 2) {
                    txttable$epsilon[iaov] <- paste(" (e = ", 
                                                    format(round(x$"Sphericity Corrections"$GGe[isph], 
                                                                 2), nsmall = 2), ")", sep = "")
                  }
                }
                else if (toupper(sph.cor) == "HF") {
                  pmaucorr <- format(round(x$"Sphericity Corrections"$"p[HF]"[isph], 
                                           3), nsmall = 3)
                  if (R.version$major < 4) {
                    levels(txttable$p) <- c(levels(txttable$p), 
                                            pmaucorr)
                  }
                  txttable[iaov, 6] = pmaucorr
                  if (corr.df == TRUE) {
                    corr.df1 <- format(round(x$"Sphericity Corrections"$HFe[isph] * 
                                               as.numeric(txttable[iaov, 3]), 
                                             2), nsmall = 2)
                    corr.df2 <- format(round(x$"Sphericity Corrections"$HFe[isph] * 
                                               as.numeric(txttable[iaov, 4]), 
                                             2), nsmall = 2)
                    txttable[iaov, 3] = corr.df1
                    if (show.eps == 1) {
                      txttable[iaov, 4] <- paste(corr.df2, 
                                                 dfsep, "e = ", format(round(x$"Sphericity Corrections"$HFe[isph], 
                                                                             2), nsmall = 2), sep = "")
                    }
                    else {
                      txttable[iaov, 4] <- corr.df2
                    }
                  }
                  else if (show.eps == 1) {
                    txttable[iaov, 4] <- paste(txttable[iaov, 
                                                        4], dfsep, "e = ", format(round(x$"Sphericity Corrections"$HFe[isph], 
                                                                                        2), nsmall = 2), sep = "")
                  }
                  else if (show.eps >= 2) {
                    txttable$epsilon[iaov] <- paste(" (e = ", 
                                                    format(round(x$"Sphericity Corrections"$HFe[isph], 
                                                                 2), nsmall = 2), ")", sep = "")
                  }
                  if (show.eps >= 2) {
                    txttable$epsilon[iaov] <- paste(" (e = ", 
                                                    format(round(x$"Sphericity Corrections"$HFe[isph], 
                                                                 2), nsmall = 2), ")", sep = "")
                  }
                }
                ajdffcts <- c(ajdffcts, eoi)
              }
            }
          }
        }
        if (length(ajdffcts) == 0) {
          note <- paste("No adjustments necessary (all p_Mauchly > ", 
                        mau.p, ").", sep = "")
        }
        else {
          if (corr.df == TRUE) {
            notedf <- " Reporting corrected degrees of freedom."
          }
          else {
            notedf <- " Reporting uncorrected degrees of freedom."
          }
          note <- paste("p-values for the following effects were ", 
                        sph.cor, "-adjusted (p_Mauchly <= ", mau.p, 
                        "): ", paste(paste(ajdffcts, collapse = "; ", 
                                           sep = ""), ".", notedf, sep = ""), sep = "")
        }
      }
      else {
        if (toupper(sph.cor) == "NO") {
          note <- "Reporting unadjusted p-values."
        }
        else if (outspher != "N/A") {
          note <- "Reporting unadjusted p-values."
        }
      }
      pcorr <- paste(", p = ", txttable$p, sep = "")
      pcorr <- gsub("p = 1.000", "p > .999", pcorr, fixed = TRUE)
      pcorr <- gsub("p = 0.000", "p < .001", pcorr, fixed = TRUE)
      pcorr <- gsub("p = 0", "p = ", pcorr, fixed = TRUE)
      if (etasq == "partial") {
        petasqcorr <- paste(", np2 = ", txttable$petasq, 
                            sep = "")
        petasqcorr <- gsub("np2 = 1.00", "np2 > .99", 
                           petasqcorr, fixed = TRUE)
        petasqcorr <- gsub("np2 = 0.00", "np2 < .01", 
                           petasqcorr, fixed = TRUE)
        petasqcorr <- gsub("np2 = 0", "np2 = ", petasqcorr, 
                           fixed = TRUE)
        if (show.eps == 3) {
          outtext <- data.frame(Effect = x$ANOVA$Effect, 
                                Text = paste("F(", txttable$df1, dfsep, txttable$df2, 
                                             ") = ", txttable$F, pcorr, petasqcorr, 
                                             txttable$epsilon, sep = ""))
        }
        else if (show.eps == 2) {
          outtext <- data.frame(Effect = x$ANOVA$Effect, 
                                Text = paste("F(", txttable$df1, dfsep, txttable$df2, 
                                             ") = ", txttable$F, txttable$epsilon, pcorr, 
                                             petasqcorr, sep = ""))
        }
        else {
          outtext <- data.frame(Effect = x$ANOVA$Effect, 
                                Text = paste("F(", txttable$df1, dfsep, txttable$df2, 
                                             ") = ", txttable$F, pcorr, petasqcorr, 
                                             sep = ""))
        }
      }
      else {
        getasqcorr <- paste(", ng2 = ", txttable$getasq, 
                            sep = "")
        getasqcorr <- gsub("ng2 = 1.00", "ng2 > .99", 
                           getasqcorr, fixed = TRUE)
        getasqcorr <- gsub("ng2 = 0.00", "ng2 < .01", 
                           getasqcorr, fixed = TRUE)
        getasqcorr <- gsub("ng2 = 0", "ng2 = ", getasqcorr, 
                           fixed = TRUE)
        if (show.eps == 3) {
          outtext <- data.frame(Effect = x$ANOVA$Effect, 
                                Text = paste("F(", txttable$df1, dfsep, txttable$df2, 
                                             ") = ", txttable$F, pcorr, getasqcorr, 
                                             txttable$epsilon, sep = ""))
        }
        else if (show.eps == 2) {
          outtext <- data.frame(Effect = x$ANOVA$Effect, 
                                Text = paste("F(", txttable$df1, dfsep, txttable$df2, 
                                             ") = ", txttable$F, txttable$epsilon, pcorr, 
                                             getasqcorr, sep = ""))
        }
        else {
          outtext <- data.frame(Effect = x$ANOVA$Effect, 
                                Text = paste("F(", txttable$df1, dfsep, txttable$df2, 
                                             ") = ", txttable$F, pcorr, getasqcorr, 
                                             sep = ""))
        }
      }
    }
    else {
      outtext <- "N/A"
    }
    x <- list(`--- ANOVA RESULTS     ------------------------------------` = outtable, 
              `--- SPHERICITY TESTS  ------------------------------------` = outspher, 
              `--- FORMATTED RESULTS ------------------------------------` = outtext)
    if (exists("note")) {
      x = c(x, `NOTE:` = note)
    }
    if (print == TRUE) {
      print(x)
    }
    else {
      x
    }
  }
}