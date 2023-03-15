
library(activityinfo)
library(tidyverse)
library(janitor)
library(pivottabler)
library(openxlsx)
library(colorspace)

options(activityinfo.interactive = FALSE)
activityInfoToken(Sys.getenv('ACTIVITY_INFO_TOKEN'))
budget_id <- "covv3selev9zzr25"
indicator_id <- "c3jsofplb96e4x51g"

df_budgets <- queryTable(budget_id,
                 "id" = "_id",
                 "Planning Year" = "cp0hc5slb3iu2g59",
                 "Sector Code Name" = "cdnqtiqlb0qhy466.cnv3sosksa3n9gh3",
                 "Organization Code Name" = "cpa1ggxlb0qhy477.c5ytxhyks7s2ygds",
                 "Output Output Key" = "cq2e3oflb0qhy488.cz9wy11l1um54a69",
                 "Total Budget Requirement" = "coj6cv0lb0qhy48e",
                 "Adolescent/Youth Budget" = "cibq5rulb0qhy48f",
                 "Refugee Budget" = "celmri7lb0qhy48g",
                 "Resilience Budget" = "cpy8fv9lb0qhy48h",
                 "To consider" = "c920wa8lbb6gebw7",
                 "Active" = "cv6oh41lb3d38yw7",
                 truncateStrings = FALSE) %>% janitor::clean_names() %>% filter(to_consider == "Yes" &
                                                                                        active == "Yes") %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% remove_rownames %>% column_to_rownames(var = "id") %>%
  mutate(budget_value = trimws(format(round(as.numeric(total_budget_requirement), 1), nsmall=1, big.mark=",")),
         budget_value_refugee = trimws(format(round(as.numeric(refugee_budget), 1), nsmall=1, big.mark=",")),
         budget_value_resilience = trimws(format(round(as.numeric(resilience_budget), 1), nsmall=1, big.mark=","))) %>%
  separate(output_output_key, into = c('output_key', 'output_text'), sep = " | ",remove = FALSE) %>%
  separate(organization_code_name, into = c('organization_key', 'organization_text'), sep = " | ",remove = FALSE) %>% select(-output_text,-organization_text)

glimpse(df_budgets)

df_sectors <- queryTable("croqgk6lal21i30nyl",
                 "Code Name" = "cnv3sosksa3n9gh3",
                 "Code" = "cr5u0neksa5cnnz1i",
                 "Name" = "cdrhlizksa3t40kd",
                 "Short Name" = "cga163eksa3todue",
                 "3RP Name" = "cwhlvvsksa4995wf",
                 "3RP Color" = "cno86d9ksa4ewnqm",
                 "3RP Secondary Color" = "con9bmelf7xi0s82",
                 "3RP" = "cedz9l4ksa4g63ct") %>% janitor::clean_names() %>% filter(x3rp == "Yes")

glimpse(df_sectors)


df <- queryTable("c3jsofplb96e4x51g",
                 "Regional Strategic Direction" = "ch02jx3lb95bphk7.crvj87klb9t8763a.cipn9kdkuxri0kqe",
                 "Inter-Sectoral Outcome" = "ch02jx3lb95bphk7.cju7981lb9t9p9fb.c7ij53wkuxsdn66v",
                 "Planning Year" = "cbd3bxalb98bxd42a",
                 "Organization Code Name" = "chstnf1lb96fok91i.c5ytxhyks7s2ygds",
                 "Sector Code Name" = "cdew5q9lb97cfa826.cnv3sosksa3n9gh3",
                 "Output Output Key" = "c4eqiztlb9w5vgw2.cz9wy11l1um54a69",
                 "Indicator Indicator Key" = "cxrmk41lb96eurt1h.cvdgro6lb95c3srm",
                 "Indicator Target" = "cbi0x2slb96gb711j",
                 "To consider" = "c91mi18lbb6jnxcc",
                 "Active" = "cmtq82flb96gnff1p")


df_indicators <- queryTable(indicator_id,
                            "id" = "_id",
                  "Regional Strategic Direction" = "ch02jx3lb95bphk7.crvj87klb9t8763a.cipn9kdkuxri0kqe",
                  "Inter-Sectoral Outcome" = "ch02jx3lb95bphk7.cju7981lb9t9p9fb.c7ij53wkuxsdn66v",           
                 "Planning Year" = "cbd3bxalb98bxd42a",
                 "Organization Code Name" = "chstnf1lb96fok91i.c5ytxhyks7s2ygds",
                 "Sector Code Name" = "cdew5q9lb97cfa826.cnv3sosksa3n9gh3",
                 "Output Output Key" = "c4eqiztlb9w5vgw2.cz9wy11l1um54a69",
                 "Indicator Indicator Key" = "cxrmk41lb96eurt1h.cvdgro6lb95c3srm",
                 "Indicator Target" = "cbi0x2slb96gb711j",
                 "To consider" = "c91mi18lbb6jnxcc",
                 "Active" = "cmtq82flb96gnff1p",
                 truncateStrings = FALSE)%>% janitor::clean_names() %>% filter(to_consider == "Yes" &
                                                                                 active == "Yes") %>% remove_rownames %>% column_to_rownames(var = "id") %>%
  separate(output_output_key, into = c('output_key', 'output_text'), sep = " | ",remove = FALSE)%>%
  separate(organization_code_name, into = c('organization_key', 'organization_text'), sep = " | ",remove = FALSE) %>% select(-output_text,-organization_text)

glimpse(df_indicators)

df_budgets_join <- left_join(df_budgets,df_indicators, by=c("planning_year","output_key","organization_key"))
glimpse(df_budgets_join)


sectors <- unique(df_budgets_join$sector_code_name.x)



wb <- createWorkbook(creator = Sys.getenv("USERNAME"))

for(i in 1:nrow(df_sectors)) {
  row <- df_sectors[i,]
  sector <- row$code_name
  sector_value <- row$x3rp_name
  color_table <- row$x3rp_color
  # define the font and colours
  simpleTheme <- list(
    fontSize="0.75em",
    headerBackgroundColor = color_table,
    headerColor = "#FFFFFF",
    cellBackgroundColor = "#FFFFFF",
    cellColor = "#000000",
    outlineCellBackgroundColor = lighten(color_table, 0.6),
    outlineCellColor = "#777777",
    totalBackgroundColor = color_table,
    totalColor = "#FFFFFF",
    borderColor = darken(color_table, 0.4)
  )
  

  pt <- PivotTable$new()
  pt$addData(df_budgets_join %>% filter(sector_code_name.x == sector))
  pt$addRowDataGroups("regional_strategic_direction",addTotal = FALSE,header = "Regional Strategic Direction")
  pt$addRowDataGroups("inter_sectoral_outcome",addTotal = FALSE,header = "Sectoral Outcome")
  pt$addRowDataGroups("sector_code_name.x",addTotal = FALSE,header = "Sector")
  pt$addRowDataGroups("output_output_key.x",addTotal = FALSE,header = "Output")
  pt$addRowDataGroups("organization_code_name.x",addTotal = FALSE,header = "Organization")
  pt$addRowDataGroups("refugee_budget",addTotal = FALSE,header = "Refugee Budget", styleDeclarations=list("xl-value-format"="$#,##0_);($#,##0)","xl-h-align"= "right"))
  pt$addRowDataGroups("resilience_budget",addTotal = FALSE,header = "Resilience Budget", styleDeclarations=list("xl-value-format"="$#,##0_);($#,##0)","xl-h-align"= "right"))
  pt$addRowDataGroups("total_budget_requirement",addTotal = FALSE,header = "Total Budget", styleDeclarations=list("xl-value-format"="$#,##0_);($#,##0)","xl-h-align"= "right"))
  pt$addRowDataGroups("indicator_indicator_key",addTotal = FALSE,header = "Indicators")
  
  pt$defineCalculation(calculationName="Target", summariseExpression="sum(indicator_target, na.rm = TRUE)", format="%.1f",
                       cellStyleDeclarations=list("xl-value-format"="#,##0"))
  pt$theme <- simpleTheme
  
  
  pt$evaluatePivot()
  
  pt$mapStyling(cells=pt$allCells, styleProperty="background-color",
                valueType="color", mapType="logic",
                mappings=list("v==0", "#F2DCDB", "v>0", "#FFFFFF"))
  
  addWorksheet(wb, toupper(sector_value))
  
  pt$writeToExcelWorksheet(wb=wb, wsName= toupper(sector_value), showRowGroupHeaders=TRUE,
                           topRowNumber=1, leftMostColumnNumber=1,
                           outputValuesAs="rawValue",
                           applyStyles=TRUE, mapStylesFromCSS=TRUE)
  
}

saveWorkbook(wb, file="Planning Details 3RP - 2023.xlsx", overwrite = TRUE)
