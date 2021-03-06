#download packages if you haven't yet
packages <- c("devtools", "eRm")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
devtools::install_github("klutometis/roxygen")
#include libraries
library("eRm")
library("devtools")
library(roxygen2)

#create package directory
setwd("parent_directory")
create("astromERM")

fn_anova_eRm <- function(object, ...){
  return(anova.eRm(object, ...))
}

fn_anova_llra <- function(object){
  return(anova(object))
}

fn_build_W <- function(X, nitems, npoints, grp_n, groupvec, itmgrps){
  return(build_W(X, nitems, npoints, grp_n, groupvec, itmgrps))
}

fn_cldeviance <- function(object, groups.gr = "rawscore", pi.hat){
  return(cldeviance(object, groups.gr = "rawscore", pi.hat))
}

fn_cmlprep <- function(X01,mt_vek,mpoints,Groups,W,gmemb){
  return(cmlprep(X01,mt_vek,mpoints,Groups,W,gmemb))
}

fn_coef.eRm <- function(object, parm = "beta", ...) {
  return(coef.eRm(object, parm="beta", ...))
}

fn_coefppar <- function(object, extrapolated = TRUE, ...) {
  return(coef.ppar(object, extrapolated = TRUE, ...))
}

fn_collapse_W <- function(W, listItems, newNames){
  return(collapse_W(W, listItems, newNames))
}

fn_confint_eRm <- function(object, parm="beta", level = 0.95, ...){
  return(confint.eRm(object, parm="beta", level = 0.95, ...))
}

fn_confint_ppar <- function(object, parm, level = 0.95, ...){
  return(confint.ppar(object, parm, level = 0.95, ...))
}

fn_gofIRT <- function(object, groups.h1, cutpoint){
  return(gofIRT(object, groups.h1 , cutpoint))
}

fn_confint_threshold <- function(object, parm, level = 0.95, ...){
  return(confint.threshold(object, parm, level = 0.95, ...))
}

fn_cwdeviance <- function(object, pi.hat){
  return(cwdeviance(object, pi.hat))
}

fn_datcheck.LRtest <- function(x, X, model){
  return(datcheck.LRtest(x, X, model))
}

fn_datcheck <- function(X, W, mpoints, groupvec, model){
  return(datcheck(X, W, mpoints, groupvec, model))
}

fn_datprep_LLTM <- function(X,W,mpoints,Groups,sum0){
  return(datprep_LLTM(X,W,mpoints,Groups,sum0))
}

fn_datprep_LPCM <- function(X,W,mpoints,Groups,sum0) {
  return(datprep_LPCM(X,W,mpoints,Groups,sum0))
}

fn_datprep_LRSM <- function(X,W,mpoints,Groups,sum0){
  return(datprep_LRSM(X,W,mpoints,Groups,sum0))
}

fn_datprep_PCM <- function(X,W,sum0){
  return(datprep_PCM(X,W,sum0))
}

fn_datprep_RM <- function(X,W,sum0) {
  return(datprep_RM(X,W,sum0))
} 

fn_datprep_RSM <- function(X,W,sum0){
  return(datprep_RSM(X,W,sum0))
}

fn_fitcml <- function(mt_ind, nrlist, x_mt, rtot, W, ngroups, gind, x_mtlist, NAstruc, g_NA, st.err, etaStart, gby){
  return(fitcml(mt_ind, nrlist, x_mt, rtot, W, ngroups, gind, x_mtlist, NAstruc, g_NA, st.err, etaStart, gby))
}

fn_gofIRT.ppar <- function(object, groups.hl = 10, cutpoint = 0.5) {
  return(gofIRT.ppar(object, groups.hl = 10, cutpoint = 0.5))
}

fn_gofIRT <- function(object, groups.hl = 10, cutpoint = 0.5){
  return(gofIRT(object, groups.hl = 10, cutpoint = 0.5))
}

fn_hoslem <- function(object, groups.hl = 10, pi.hat){
  return(hoslem(object, groups.hl = 10, pi.hat))
}

fn_IC <- function(object){
  return(IC(object))
}

fn_invalid <- function(x){
  return(invalid(x))
}

fn_i_info<-function(hvec,itembeta,theta){
  return(i_info(hvec,itembeta,theta))
}

fn_itemfit_ppar <- function(object, x, visible){
  return(itemfit.ppar(object, x, visible))
}

fn_itemfit <- function(object){
  return(itemfit(object))
}

fn_item_info <- function(ermobject, theta, hvec, itembeta){
  return(item_info(ermobject, theta, hvec, itembeta))
}

fn_labeling_internal <- function(model,X,X01,W,etapar,betapar,mpoints,ngroups){
  return(labeling.internal(model,X,X01,W,etapar,betapar,mpoints,ngroups))
}

fn_llra_datprep<-function(X, mpoints, groups, baseline=NULL){
  return(llra.datprep(X, mpoints, groups, baseline=NULL))
}

fn_LLRA <- function(X, W, mpoints, groups, baseline, itmgrps, x){
  return(LLRA(X, W, mpoints, groups, baseline, itmgrps, x))
}

fn_LLTM <- function(X, W, mpoints = 1, groupvec = 1, se = TRUE, sum0 = TRUE, etaStart){
  return(LLTM(X, W, mpoints = 1, groupvec = 1, se = TRUE, sum0 = TRUE, etaStart))
}

fn_likLR <- function(X, W, mpoints, Groups, model, st.err, sum0, etaStart){
  return(likLR(X, W, mpoints, Groups, model, st.err, sum0, etaStart))
}

fn_llra_datprep <- function(X, mpoints, groups, baseline){
  return(llra.datprep(X, mpoints, groups, baseline))
}

fn_logLik_eRm <- function(object,...){
  return(logLik.eRm(object,...))
}

fn_logLik.ppar <- function(object,...){
  return(logLik.ppar(object,...))
}

fn_LPCM <- function(X, W, mpoints = 1, groupvec = 1, se = TRUE, sum0 = TRUE, etaStart) {
  return(LPCM(X, W, mpoints = 1, groupvec = 1, se = TRUE, sum0 = TRUE, etaStart))
}

fn_LRSM <- function(X, W, mpoints = 1, groupvec = 1, se = TRUE, sum0 = TRUE, etaStart) {
  return(LRSM(X, W, mpoints = 1, groupvec = 1, se = TRUE, sum0 = TRUE, etaStart))
}

fn_LRtest <- function(object, splitcr = "median", se = TRUE){
  return(LRtest(object, splitcr = "median", se = TRUE))
}

fn_LRtest_Rm <-function(object, splitcr = "median", se = TRUE) {
  return(LRtest.Rm(object, splitcr = "median", se = TRUE))
}

fn_MLoef <- function(robj, splitcr="median"){
  return(MLoef(robj, splitcr="median"))
}

fn_model_matrix_eRm <- function(object,...){
  return(model.matrix.eRm(object,...))
}

fn_NPtest <- function(obj, n=NULL, method="T1", ...){
  return(NPtest(obj, n=NULL, method="T1", ...))
}

fn_PCM <- function(X, W, se = TRUE, sum0 = TRUE, etaStart){
  return(PCM(X, W, se = TRUE, sum0 = TRUE, etaStart))
}

fn_performance <- function(prediction.obj, measure, x.measure="cutoff", ...) {
  return(performance(prediction.obj, measure, x.measure="cutoff", ...))
}

fn_person_parameter_eRm <- function(object){
  return(person.parameter.eRm(object))
}

fn_person_parameter <- function(object){
  return(person.parameter(object))
}

fn_personfit_ppar <- function(object){
  return(personfit.ppar(object))
}

fn_personfit <- function(object){
  return(personfit(object))
}

fn_phi_range <- function(mat){
  return(phi.range(mat))
}

fn_pifit_internal <- function(object){
  return(pifit.internal(object))
}

fn_plist_internal <- function(object, theta){
  return(plist.internal(object, theta))
}

fn_plot.ppar <- function(x, xlab = "Person Raw Scores", ylab = "Person Parameters (Theta)", main = NULL, ...){
  return(plot.ppar(x, xlab = "Person Raw Scores", ylab = "Person Parameters (Theta)", main = NULL, ...))
}

fn_plotCI <- function (x,
                    y = NULL,
                    uiw,
                    liw = uiw,
                    ui,
                    li,
                    
                    err='y',
                    ylim=NULL,
                    xlim=NULL,
                    type="p",
                    
                    col=par("col"),
                    barcol=col,
                    pt.bg = par("bg"),
                    
                    sfrac = 0.01,
                    gap=1,
                    
                    lwd=par("lwd"),
                    lty=par("lty"),
                    
                    labels=FALSE,
                    
                    add=FALSE,
                    xlab,
                    ylab,
                    
                    minbar,
                    maxbar,
                    ...
){
  return(plotCI(x,
                             y = NULL,
                             uiw,
                             liw = uiw,
                             ui,
                             li,
                             
                             err='y',
                             ylim=NULL,
                             xlim=NULL,
                             type="p",
                             
                             col=par("col"),
                             barcol=col,
                             pt.bg = par("bg"),
                             
                             sfrac = 0.01,
                             gap=1,
                             
                             lwd=par("lwd"),
                             lty=par("lty"),
                             
                             labels=FALSE,
                             
                             add=FALSE,
                             xlab,
                             ylab,
                             
                             minbar,
                             maxbar,
                             ...
  ))
}

fn_plotDIF <- function(object, item.subset=NULL, gamma = 0.95, main=NULL,
                    xlim=NULL, xlab=" ", ylab=" ", col=NULL, distance,
                    splitnames=NULL, leg=FALSE, legpos="bottomleft", ...){
  return(plotDIF(object, item.subset=NULL, gamma = 0.95, main=NULL,
                             xlim=NULL, xlab=" ", ylab=" ", col=NULL, distance,
                             splitnames=NULL, leg=FALSE, legpos="bottomleft", ...))
}

fn_plotGOF_LR <- function(
  x,
  beta.subset = "all",
  main = "Graphical Model Check",
  xlab,
  ylab,
  tlab = "item",
  xlim,
  ylim,
  type = "p",
  pos = 4,
  conf = NULL,
  ctrline = NULL,
  asp = 1,
  x_axis = TRUE,
  y_axis = TRUE,
  set_par = TRUE,
  reset_par = TRUE,
  ...
){
  return(plotGOF.LR(
    x,
    beta.subset = "all",
    main = "Graphical Model Check",
    xlab,
    ylab,
    tlab = "item",
    xlim,
    ylim,
    type = "p",
    pos = 4,
    conf = NULL,
    ctrline = NULL,
    asp = 1,
    x_axis = TRUE,
    y_axis = TRUE,
    set_par = TRUE,
    reset_par = TRUE,
    ...
  ))
}
  
fn_plotGOF <- function(x, ...) {
  return(plotGOF(x, ...))
}

fn_plotGR <- function(object,...){
  return(plotGR(object,...))
}

fn_plotICC <- function(object, ...){
  return(plotICC(object, ...))
}

fn_plotICC_Rm <- function(
  object,
  item.subset = "all",
  empICC = NULL,
  empCI = NULL,
  mplot = NULL,    # ask,mplot added rh 2007-12-01
  xlim = c(-4,4),
  ylim = c(0,1),
  xlab = "Latent Dimension",
  ylab = "Probability to Solve",
  main = NULL,       # main rh 2010-03-06
  col = NULL,
  lty = 1,
  legpos = "left",
  ask = TRUE,
  ...)
{
  return(plotICC.Rm(
    object,
    item.subset = "all",
    empICC = NULL,
    empCI = NULL,
    mplot = NULL,    # ask,mplot added rh 2007-12-01
    xlim = c(-4,4),
    ylim = c(0,1),
    xlab = "Latent Dimension",
    ylab = "Probability to Solve",
    main = NULL,       # main rh 2010-03-06
    col = NULL,
    lty = 1,
    legpos = "left",
    ask = TRUE,
    ...))
}

fn_plotINFO <- function(ermobject, type = "both", theta = seq(-6, 6, length.out = 1001L), ...){
  return(plotINFO(ermobject, type = "both", theta = seq(-6, 6, length.out = 1001L), ...))
}
  
fn_plotjointICC_dRm <- function(
  object,
  item.subset = "all",
  legend = TRUE,
  xlim = c(-4, 4),
  ylim = c(0, 1),
  xlab = "Latent Dimension",
  ylab = "Probability to Solve",
  lty = 1,
  legpos="topleft",
  main = "ICC plot",
  col = NULL,
  ...
){ return(plotjointICC.dRm(
    object,
    item.subset = "all",
    legend = TRUE,
    xlim = c(-4, 4),
    ylim = c(0, 1),
    xlab = "Latent Dimension",
    ylab = "Probability to Solve",
    lty = 1,
    legpos="topleft",
    main = "ICC plot",
    col = NULL,
    ...
  ))
}

fn_plotjointICC <- function(object, ...){
  return(plotjointICC(object, ...))
}

fn_plotPImap <-function(object, item.subset="all", sorted = FALSE, main="Person-Item Map",
           latdim="Latent Dimension", pplabel="Person\nParameter\nDistribution",
           cex.gen=0.7, xrange=NULL,
           warn.ord=TRUE, warn.ord.colour="black",
           irug=TRUE, pp=NULL){
  return(plotPImap(object, item.subset="all", sorted = FALSE, main="Person-Item Map",
                    latdim="Latent Dimension", pplabel="Person\nParameter\nDistribution",
                    cex.gen=0.7, xrange=NULL,
                    warn.ord=TRUE, warn.ord.colour="black",
                    irug=TRUE, pp=NULL))
}

fn_plotPWmap <- function(object, pmap=FALSE, imap=TRUE, item.subset="all", person.subset="all",
           mainitem="Item Map", mainperson="Person Map",
           mainboth="Item/Person Map", latdim="Latent Dimension",
           tlab="Infit t statistic", pp=NULL, cex.gen=0.6, cex.pch=1,
           person.pch=1, item.pch=16, personCI=NULL, itemCI=NULL, horiz=FALSE) {
  return(plotPWmap(object, pmap=FALSE, imap=TRUE, item.subset="all", person.subset="all",
                    mainitem="Item Map", mainperson="Person Map",
                    mainboth="Item/Person Map", latdim="Latent Dimension",
                    tlab="Infit t statistic", pp=NULL, cex.gen=0.6, cex.pch=1,
                    person.pch=1, item.pch=16, personCI=NULL, itemCI=NULL, horiz=FALSE))
}

fn_plotTR <-function(object,...){
  return(plotTR(object,...))
}

fn_pmat_default <- function(object){
  return(pmat.default(object))
}

fn_pmat_ppar <- function(object){
  return(pmat.ppar(object))
}

fn_pmat <- function(object){
  return(pmat(object))
}

fn_predict_ppar <- function(object, cutpoint = "randomized", ...){
  return(predict.ppar(object, cutpoint = "randomized", ...))
}

fn_prediction <- function(predictions, labels, label.ordering=NULL) {
  return(prediction(predictions, labels, label.ordering=NULL))
}

fn_print_eRm <- function(x,...)  { 
  return(print.eRm(x,...))
}

fn_print_gof <- function(x, ...){
  return(print.gof(x, ...))
}

fn_print_ICr <- function(x,...){
  return(print.ICr(x,...))
}

fn_print_ifit <- function(x, visible=TRUE, ...){
  return(print.ifit(x, visible=TRUE, ...))
}

fn_print_llra <- function(x,...){
  return(print.llra(x,...))
}

fn_print_logLik_eRm <- function (x, digits = getOption("digits"),...){
  return(print.logLik.eRm(x, digits = getOption("digits"),...))
}

fn_print_logLik_ppar <- function (x, digits = getOption("digits"),...){
  return(print.logLik.ppar(x, digits = getOption("digits"),...))
}

fn_print_LR <- function(x,...) {
  return(print.LR(x,...))
}

fn_print_MLoef <- function(x,...){
  return(print.MLoef(x,...))
}

fn_print_pfit <- function(x, visible=TRUE, ...){
  return(print.pfit(x, visible=TRUE, ...))
}

fn_print_ppar <- function(x,...){
  return(print.ppar(x,...))
}

fn_print_resid <- function(x,...){
  return(print.resid(x,...))
}

fn_print_step <- function(x, ...){
  return(print.step(x, ...))
}

fn_print_summary_llra <- function(x,...){
  return(print.summary.llra(x,...))
}

fn_print_threshold <- function(x,...){
  return(print.threshold(x,...))
}

fn_print_wald <- function(x,...) {
  return(print.wald(x,...))
}

fn_residuals_ppar <- function(object,...){
  return(residuals.ppar(object,...))
}

fn_RM <- function(X, W, se = TRUE, sum0 = TRUE, etaStart){
  return(RM(X, W, se = TRUE, sum0 = TRUE, etaStart))
}

fn_rostdeviance <- function(object){
  return(rostdeviance (object))
}

fn_rsampler <- function(inpmat,controls=rsctrl()){
  return(rsampler(inpmat,controls=rsctrl()))
}

fn_rsctrl <- function(burn_in=100, n_eff=100, step=16, seed=0, tfixed=FALSE){
  return(rsctrl(burn_in=100, n_eff=100, step=16, seed=0, tfixed=FALSE))
}

fn_rserror <- function(err){
  return(rserror(err))
}

fn_rsextrmat <- function(RSobj, mat.no = 1){
  return(rsextrmat(RSobj, mat.no = 1))
}

fn_rsextrobj <- function(RSobj,start=1,end=8192) {
  return(rsextrobj(RSobj,start=1,end=8192))
}

fn_RSM <- function(X, W, se = TRUE, sum0 = TRUE, etaStart) {
  return(RSM(X, W, se = TRUE, sum0 = TRUE, etaStart))
}

fn_Rsquared <- function(object, pi.hat){
  return(Rsquared(object, pi.hat))
}

fn_rstats <- function(RSobj,userfunc,...){
  return(rstats(RSobj,userfunc,...))
}

fn_rsunpack <- function(x,n,k,nwords,userfunc,...){
  return(rsunpack(x,n,k,nwords,userfunc,...))
}

fn_SepRel <- function (pobject){ 
  return(SepRel(pobject))
}

fn_sim_2pl <- function(persons, items, discrim = 0.25, seed = NULL, cutpoint = "randomized"){
  return(sim.2pl(persons, items, discrim = 0.25, seed = NULL, cutpoint = "randomized"))
}

fn_sim_locdep <- function(persons, items, it.cor = 0.25, seed = NULL, cutpoint = "randomized"){
  return(sim.locdep(persons, items, it.cor = 0.25, seed = NULL, cutpoint = "randomized"))
}

fn_sim_rasch <-function(persons, items, seed = NULL, cutpoint = "randomized"){
  return(sim.rasch(persons, items, seed = NULL, cutpoint = "randomized"))
}

fn_sim_xdim <- function(persons, items, Sigma, weightmat, seed = NULL, cutpoint = "randomized"){
  return(sim.xdim(persons, items, Sigma, weightmat, seed = NULL, cutpoint = "randomized"))
}

fn_stepwiseIt_eRm <- function(object, criterion = list("itemfit"), alpha = 0.05, verbose = TRUE,
                              maxstep = NA){
  return(stepwiseIt.eRm(object, criterion = list("itemfit"), alpha = 0.05, verbose = TRUE,
                                    maxstep = NA))
}

fn_stepwiseIt <- function(object, criterion = list("itemfit"), alpha = 0.05, verbose = TRUE,
           maxstep = NA) {
  return(stepwiseIt(object, criterion = list("itemfit"), alpha = 0.05, verbose = TRUE,
                    maxstep = NA))
}

fn_summary_eRm <- function(object,...){
  return(summary.eRm(object,...))
}

fn_summary_gof <- function(object, ...){
  return(summary.gof(object, ...))
}

fn_summary_llra <- function(object, level=0.95, ...){
  return(summary.llra(object, level=0.95, ...))
}

fn_summary_LR <- function(object,...){
  return(summary.LR(object,...))
}

fn_summary_MLoef <- function(object, ...){
  return(summary.MLoef(object, ...))
}

fn_summary_ppar <- function(object,...){
  return(summary.ppar(object,...))
}

fn_summary_RSctr <- function(object,...){
  return(summary.RSctr(object,...))
}

fn_summary_RSmpl <- function(object,...){
  return(summary.RSmpl(object,...))
}

fn_summary_RSmplext <- function(object,...){
  return(summary.RSmplext(object,...))
}

fn_summary_threshold <- function(object,...){
  return(summary.threshold(object,...))
}

fn_test_info <- function(ermobject,theta=seq(-5,5,0.01)){
  return(test_info(ermobject,theta=seq(-5,5,0.01)))
}

fn_thresholds_eRm <- function(object){
  return(thresholds.eRm(object))
} 

fn_thresholds <- function(object){
  return(thresholds(object))
}

fn_vcov_eRm <- function(object,...){
  return(vcov.eRm(object,...))
}

fn_Waldtest <- function(object,splitcr="median"){
  return(Waldtest(object,splitcr="median"))
}

fn_Waldtest_Rm <- function(object, splitcr = "median"){
  return(Waldtest.Rm(object, splitcr = "median"))
}

setwd("./cats")
document()

#source: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
