if(!require("tidyverse")) install.packages("tidyverse")
if(!require("htmlTable")) install.packages("htmlTable")
if(!require("broom")) install.packages("broom")

#' Title
#'
#' @param dat1 a data set
#' @param stratas a variable
#' @param catVars a variable
#' @param conVars a variable
#'
#' @return
#' @export
#'
#' @examples
tabf = function(dat1, stratas, catVars, conVars){
  options(dplyr.summarise.inform = FALSE)
  varOrder = tibble("variables"=c(catVars, conVars)) %>%
    mutate(order = row_number())

  catTab = dat1 %>%
    select(stratas, all_of(catVars)) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by( variables, values) %>%
    count(!!sym(stratas)) %>%
    mutate(prob = n/sum(n),
           smry= sprintf("%.0f (%.1f%%)", n, prob*100)
    ) %>%
    select(-n, -prob) %>%
    ungroup() %>%
    pivot_wider(names_from = stratas, values_from =smry)

  conTab =
    dat1 %>%
    select(stratas, all_of(conVars)) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by( !!sym(stratas), variables) %>%
    summarise(avg = mean(values, na.rm =TRUE),
              std = sd(values, na.rm =TRUE)
    ) %>%
    mutate(smry  = sprintf("%.1f\u00b1%.1f", avg, std)) %>%
    select(stratas, variables, smry)%>%
    ungroup() %>%
    pivot_wider(names_from = stratas, values_from =smry) %>%
    mutate(values ="")
  tabDat = rbind(catTab, conTab)


  catPvalue =
    dat1 %>%
    select(stratas, catVars) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by(variables, values) %>%
    count(!!sym(stratas)) %>%
    pivot_wider(names_from = stratas, values_from =n) %>%
    ungroup() %>%
    select(-values) %>%
    nest(dat = -variables) %>%
    mutate(
      fit = map(dat,
                ~chisq.test(.x)),
      tidied = map(fit, tidy)
    ) %>%
    unnest(tidied) %>%
    select(variables, p.value) %>%
    mutate(p.value = ifelse(p.value <0.001, "<0.001", sprintf("%.3f", p.value)))

  conPvalue=dat1 %>%
    select(stratas, conVars) %>%
    pivot_longer(-c(stratas), names_to = "variables", values_to ="values")%>%
    group_by( variables, values) %>%
    count(!!sym(stratas)) %>%
    pivot_wider(names_from = stratas, values_from =n) %>%
    ungroup() %>%
    select(-values) %>%
    nest(dat = -variables) %>%
    mutate(
      fit = map(dat,
                ~t.test(.x)),
      tidied=map(fit, tidy)
    ) %>%
    unnest(tidied) %>%
    select(variables, p.value) %>%
    mutate(p.value = ifelse(p.value <0.001, "<0.001", sprintf("%.3f", p.value)))
  tabPvalue = rbind(catPvalue, conPvalue)

  tab1 = tabDat %>%
    left_join(tabPvalue, by=c("variables")) %>%
    left_join(varOrder, by = c("variables")) %>%
    arrange(order, values) %>%
    group_by(variables) %>%
    mutate(ranks = row_number()) %>%
    mutate(p.value   = ifelse(ranks==min(ranks), p.value,   "")) %>%
    mutate(variables = ifelse(ranks==min(ranks), variables, "")) %>%
    ungroup() %>%
    select(-order, -ranks)%>%
    mutate(values = str_replace(values, "[:digit:]\\.", ""))
  return(tab1)
}  %>% suppressWarnings()

#' Title
#'
#' @param mod a logistic regression model
#'
#' @return
#' @export
#'
#' @examples
modsmryf=function(mod) {
  cbind(mod$coefficients %>% exp(.), confint.default(mod)%>% exp(.), mod %>% tidy() %>% select(p.value))}

#' Title
#'
#' @param a model of logistic regression
#'
#' @return
#' @export
#'
#' @examples
oddf=function(a){
  if(!missing(a)){
    mm = modsmryf(a)
    mm1 = mm%>%
      data.frame() %>%
      setNames(c("or", "ll", "ul", "pvalue")) %>%
      mutate(keys=rownames(mm))
    if(!any(is.na(a$xlevels))){
      t1 = a$xlevels
      bm1 = map(1:length(t1),function(x){tibble(variables= names(t1)[x], values = t1[[x]])}) %>% do.call(rbind, .)
    } else {
      t1 = data.frame();bm1=data.frame()
    }
    if(nrow(a$model %>% select(where(is.numeric))%>% unique()) >0){
      bm2 = a$model %>% slice(1:2)%>%select(where(is.numeric))%>% pivot_longer(-c()) %>% select(variables = name) %>% mutate(values="") %>% unique()
    } else {
      bm2 = data.frame()
    }
    bm0 = rbind(bm1, bm2) %>% mutate(keys= paste0(variables, values))

    atab= bm0 %>%
      left_join(mm1, by=c("keys")) %>%
      mutate(OR95CI = case_when(
        is.na(or) ~ "<i>1.00 (reference)</i>",
        pvalue < 0.05 ~ sprintf("<b>%.2f (%.2f-%.2f)</b>", round(or, 2), round(ll, 2), round(ul, 2)),
        TRUE ~ sprintf("%.2f (%.2f-%.2f)", round(or, 2), round(ll, 2), round(ul, 2))
      )) %>%
      mutate(values = case_when(
        pvalue <0.05 ~ sprintf("<b>%s</b>", values),
        TRUE ~ values
      )) %>%
      select(variables, values, OR95CI)
    return(atab)
  } else {
    atab = data.frame("variables"=c(NA), "values"=c(NA), "OR95CI"=c(NA))
    return(atab)
  }
} %>% suppressWarnings()


#' Title
#'
#' @param ... model of logistic regression
#'
#' @return
#' @export
#'
#' @examples
oddsf= function(...){
  arglist = list(...)
  #mod_list = mget(ls()) %>%
  #      list.filter(length(.)>1)
  tt = map(arglist, oddf) %>%
    reduce(full_join, by=c("variables", "values"))
  vl = c(length(tt)-2)
  tt = tt %>% setNames(c("Variables", "Values", paste0("Model.", as.roman(1:vl))))
  return(tt)
}


#' Title
#'
#' @param ... models of logistic regression
#'
#' @return
#' @export
#'
#' @examples
oddsTabf = function(...){
  arglist = list(...)
  mod1 = arglist[[1]]
  tt = map(arglist, oddf) %>%
    reduce(full_join, by=c("variables", "values"))
  vl = c(length(tt)-2)
  ys =  mod1$formula[2] %>% as.character() %>% str_replace(., "\\=\\=", "being reference of") %>%
    str_replace_all(., '\\"', "")
  tt = tt %>% setNames(c("Variables", "Values", paste0("Model.", as.roman(1:vl))))
  tt %>%  `rownames<-`(NULL) %>%
    group_by(Variables) %>%
    mutate(rank = row_number()) %>%
    mutate(Variables = ifelse(rank == min(rank), Variables, "")) %>%
    mutate_at(., vars(starts_with("Model")), ~replace(., is.na(.), "")) %>%
    ungroup() %>% select(-rank) %>%
    addHtmlTableStyle(align = 'll') %>%
    htmlTable(
      caption = sprintf("Table. OR(95%%CI) for %s", ys)

    )

}

