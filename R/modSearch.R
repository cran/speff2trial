modSearch <- function(formula, x, y, endpoint, method, optimal){
	source <- data.frame(x,y)
	if (NCOL(x)==1){ xnames <- as.character(formula[[3]]) 
	} else { xnames <- colnames(x) }
	colnames(source) <- c(xnames, as.character(formula[[2]]))
	family <- ifelse(endpoint=="quantitative", "gaussian", "binomial")
	if (NCOL(x)>1){
		if (endpoint=="quantitative"){
			search <- summary(regsubsets(x=x, y=y, method=method, really.big=NCOL(x)>50))
		} else {
			w <- glm(formula, family=binomial, data=source)$weights
			search <- summary(regsubsets(x=x, y=y, weights=w, method=method, really.big=NCOL(x)>50))
		}
		if (optimal!="rsq"){ opt <- which(search[[optimal]]==min(search[[optimal]]))
		} else { opt <- which(search[[optimal]]==max(search[[optimal]])) }
		idx <- search$which[opt,-1]
		names <- as.character(na.omit(ifelse(idx, xnames, NA)))
		rsq <- search$rsq[opt]
		mod <- glm(as.formula(paste(formula[[2]],"~",paste(names,collapse="+"))), family=family, data=source)
	} else {
		mod <- glm(formula, family=family, data=source)
		rsq <- NULL
		names <- xnames
	}
	list(mod=mod, rsq=rsq, names=names)
}
