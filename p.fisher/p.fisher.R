#R CMD BATCH prettyPlot.R

p.fisher <- function(x,y,threshold) {

#add: prop.test, binom.test

method      <- c("pearson",   "kendall", "spearman")
alternative <- c("two.sided", "less",    "greater")
tf          <- c(TRUE, FALSE)

p_values     <- c()
test_strings <- c()
i <- 1

#cat("test(parameters)\testimate/statistic\tp.value\n")
for(m in method){
      for(a in alternative){
      	    cor<-cor.test(x, y, method = m, alternative = a)
	    #cat("cor.test(method=",m,",alternative=", a ,")\t", cor$estimate, "\t", cor$p.value, "\n")
            p_values[i] <- cor$p.value
	    test_strings[i] <- paste("cor.test(x,y,method=",m,",alternative=",a,")",sep="")
            i<-i+1
      }
}

for(a in alternative){
            ks<-ks.test(x, y,alternative = a)
	    #cat("ks.test(alternative=", a ,")\t", ks$statistic, "\t", ks$p.value, "\n")
            p_values[i] <- ks$p.value
	    test_strings[i] <- paste("ks.test(x,y,alternative=",a,")",sep="")
            i<-i+1
}

for(a in alternative){
            vt<-var.test(x, y,alternative = a)
	    #cat("var.test(alternative=", a ,")\t", vt$statistic, "\t", vt$p.value, "\n")
            p_values[i] <- vt$p.value
	    test_strings[i] <- paste("var.test(x,y,alternative=",a,")",sep="")
            i<-i+1
}

for(a in alternative){
	    tt<-t.test(x, alternative = a)
            #cat("t.test(x,alternative=", a ,")\t", tt$statistic, "\t", tt$p.value, "\n")
            p_values[i] <- tt$p.value
	    test_strings[i] <- paste("t.test(x,alternative=",a,")",sep="")
	    #, var.equal = FALSE
            i<-i+1
}

for(a in alternative){
	    tt<-t.test(y, alternative = a)
            #cat("t.test(y,alternative=", a ,")\t", tt$statistic, "\t", tt$p.value, "\n")
	    test_strings[i] <- paste("t.test(y,alternative=",a,")",sep="")
            p_values[i] <- tt$p.value
            i<-i+1
}

for(a in alternative){            
      for(l in tf){	    
      	    for(v in tf){	    

	    	  tt<-t.test(x, y, alternative = a, paired=l, var.equal=v)
            	  #cat("t.test(alternative=", a ,", paired=", l, ")\t", tt$statistic, "\t", tt$p.value, "\n")
            	  p_values[i] <- tt$p.value
	    	  test_strings[i] <- paste("t.test(x,y,alternative=",a,",paired=",l,",var.equal=",v,")",sep="")
            	  i<-i+1
	    }
       }
}

for(a in alternative){            
      for(l in tf){	    
	    wt<-wilcox.test(x, y, alternative = a, paired=l)
            #cat("wilcox.test(alternative=", a ,", paired=", l, ")\t", wt$statistic, "\t", wt$p.value, "\n")
            p_values[i] <- wt$p.value
	    test_strings[i] <- paste("wilcox.test(x,y,alternative=",a,",paired=",l,")",sep="")
            i<-i+1
      }

}

#friedman.test

nn<-length(x)
g<-c( matrix(0,ncol=nn), matrix(1,ncol=nn) )

kt<-kruskal.test(c(x,y),g)
#cat("kruskal.test(c(x,y),g)\t", kt$statistic, "\t", kt$p.value, "\n")
p_values[i] <- kt$p.value
test_strings[i] <- paste("kruskal.test(c(x,y),g)",sep="")
i<-i+1

an<-anova(lm(y ~ x))
#cat("anova(lm(y~x))\t", an$Sum[1], "\t", an$Pr[1], "\n")
p_values[i] <- an$Pr[1]
test_strings[i] <- paste("anova(lm(x~y))",sep="")
i<-i+1

for(l in tf){
      cs<-chisq.test(table(x, y), correct = l)
      #cat("chisq.test(table(correct=", l, ")\t", cs$statistic, "\t", cs$p.value, "\n")
      p_values[i] <- cs$p.value
      test_strings[i] <- paste("chisq.test(table(x,y),correct=",l,")",sep="")
      i<-i+1
}


#1-sample tests:

s<-shapiro.test(x)
#cat("shapiro.test()\t", s$statistic, "\t", s$p.value, "\n")
p_values[i] <- s$p.value
test_strings[i] <- paste("shapiro.test(x)",sep="")
i<-i+1

s<-shapiro.test(y)
#cat("shapiro.test()\t", s$statistic, "\t", s$p.value, "\n")
p_values[i] <- s$p.value
test_strings[i] <- paste("shapiro.test(y)",sep="")
i<-i+1

#summary(p_values)

result<-matrix(data = as.numeric(p_values  < threshold), ncol = 1, dimnames = list(c(test_strings), c("isSignificant")) )

#colnames(result) <- c("test","p.value")
#cat("Number of tests: [", i, "]\n")
#write.table(result, file='') 
return(result)

}



numTests <- p.fisher(  1:10,  2*(1:10)+0.1, 0.001);
numTests <- length(numTests)
counts <- matrix(0, ncol = 4, nrow = numTests)

colnames(counts) <- c("normal", "uniform", "exponential", "sum")
loops<-10000
for(l in 1:loops){
      threshold<-0.05; 
      n<-50; 
      y  <-rnorm(n); 
      x  <-rnorm(n); 
      xx <-runif(n);
      yy <-runif(n);
      xxx<-rexp(n);
      yyy<-rexp(n);
      #cat("normal vs normal")
      r<-p.fisher(  x,  y,threshold);
      counts[,1] <- counts[,1] + r
      #cat("uniform vs uniform")
      r<-p.fisher( xx, yy,threshold);
      counts[,2] <- counts[,2] + r
      #cat("exponential vs exponential")
      r<-p.fisher(xxx,yyy,threshold);
      counts[,3] <- counts[,3] + r
}
rownames(counts) <- rownames(r)
counts[,4] <- counts[,1]+counts[,2]+counts[,3]

ss<-sort(counts[,4],index.return=T)
counts[ss$ix,]






