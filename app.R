#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(rhandsontable)
library(DT)
library(activityinfo)
library(tidyverse)
library(janitor)
library(bslib)
library(shinymanager)
library(showtext) # Needed for custom font support
library(thematic)
library(openxlsx)
library(DiagrammeR)
library(waiter)
library(validate)
library(spsComps)
library(htmltools)
library(formattable)
library(summaryBox)
library(gt)

p_year <- Sys.getenv('PLANNING_YEAR')

# Setup the bslib theme object
my_theme <- bs_theme(
  base_font = font_google("Open Sans"),
  heading_font = font_google("Source Sans Pro"),
  bg = "#FFFFFF",
  fg = "#000000",
  success = "#00B398",
  info = "#0072BC",
  warning = "#FAEB00",
  danger = "#EF4A60",
  primary = "#0a6e4f",
  secondary = "#30bced") %>%
  bs_add_variables(
    # low level theming
    "font-size-base" = "0.85rem")

# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")
activityInfoLogin(Sys.getenv('ACTIVITY_INFO_UN'),
                  Sys.getenv('ACTIVITY_INFO_TOKEN'),
                  savePassword = FALSE)

organization_form_id <- "cajkf0zlal21i30nyy"

print("organization_first")
organization_first <- queryTable(
  organization_form_id,
  "id" = "_id",
  "Code Name" = "c5ytxhyks7s2ygds",
  "Acronym" = "cmguuq9ks78tw006",
  "Code" = "c2lrasyks78sxm75",
  "Access Token" = "czc76vylb16btf31c",
  "Name" = "co30eh7ks78upg57",
  "Appealing" = "cd6kakml733j7ga7",
  filter = "cd6kakml733j7ga7 == 'Yes'",
  "Active" = "comzfppks7rsqhvb",
  truncate.strings = FALSE
) %>% janitor::clean_names() %>% filter(appealing == "Yes" &
                                          active == "Yes") %>% remove_rownames %>% column_to_rownames(var = "id")

# define some credentials
credentials <- data.frame(
  user = organization_first$code,
  # mandatory
  password = organization_first$access_token,
  # mandatory
  comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# In global.R for example:
set_labels(
  language = "en",
  "Please authenticate" = "Select your organization",
  "Username:" = "What's your organization ID:",
  "Password:" = "Enter your organization Token:"
)

rules_target <- validator(indicator_target > 0)
names(rules_target) <-
  c("indicator_target_should_be_above_0")
description(rules_target) <-
  c("Indicator target must be greater than 0")

rules_budget <- validator(
  youth_budget <= budget_requirement,
  refugee_budget + resilience_budget == budget_requirement,
  youth_budget >=  0,
  refugee_budget >=  0,
  resilience_budget >= 0
)
names(rules_budget) <-
  c(
    "youth_budget_should_be_less_than_the_budget_requirements",
    "refugee_budget_plus_resilience_budget_should_be_equal_to_the_budget_requirement",
    "youth_budget_should_more_or_equal_to_0",
    "refugee_budget_should_more_or_equal_to_0",
    "resilience_budget_should_more_or_equal_to_0"
  )
description(rules_budget) <-
  c(
    "It is essential that the money that should be allocated to young people be smaller than the required budget.",
    "It is important that the money that should be allocated to refugees and the budget allocated to developing resilience together equal the required total budget.",
    "The budget for young people should be more than or equal to zero.",
    "The refugee budget should be more than or equal to zero.",
    "The resilience budget should be more than or equal to zero."
  )


rules_contributions <- validator(refugee_budget >=  0,
                                 resilience_budget >=  0,
                                 year < planning_year,
                                 !is.na(year))
names(rules_contributions) <-
  c(
    "refugee_budget_contributions_should_be_more_or_equal_to_0",
    "resilience_budget_contributions_should_be_more_or_equal_to_0",
    "contribution_year_should_be_before_the_planning_year",
    "contribution_year_should_be_provided"
  )
description(rules_contributions) <-
  c(
    "Contributions to the refugee budget should be more than or equal to 0.",
    "Contributions to the resilience budget should be more than or equal to 0.",
    "The year of budget contributions need to come before the year of planning.",
    "The year of budget contributions should be provided."
  )
rules_comparaison <-
  validator(
    budget_requirement_contributions <= budget_requirement_plan,
    refugee_budget_plan >=  refugee_budget_contributions,
    resilience_budget_plan >=  resilience_budget_contributions
  )
names(rules_comparaison) <-
  c(
    "existing_contributions_should_less_or_equal_to_the_budget_requirement",
    "existing_refugee_contributions_should_less_or_equal_to_the_refugee_budget_requirement",
    "existing_resilience_contributions_should_less_or_equal_to_the_resilience_budget_requirement"
  )
description(rules_comparaison) <-
  c(
    "The amount of the current contributions should be less than or equal to the amount needed for the budget.",
    "The current contributions to refugees should be less than or equal to the amount needed for the refugees' budget.",
    "The current contributions to resilience should to be lower than or equal to the demand that is established by the resilience budget."
  )

present_mistakes <- function(data, rules) {
  out  <- confront(data, rules)
  summary_errors <- summary(out)
  lapply(1:length(out), function(i) {
    rule_details <-  rules[[i]]
    error_details <- summary_errors[i, ]
    out_line <- out[i]
    
    rule_name <- rule_details@name
    rule_description <- rule_details@description
    print(rules[i])
    rows_with_issues <- violating(data, rules[i])
    print(rows_with_issues)
    items <- error_details$items
    nNA <- error_details$nNA
    passes <- error_details$passes
    fails <- error_details$fails
    wellPanel(
      h3(rule_name,  class = "pb-2 mb-4 text-danger border-bottom border-danger"),
      h4("Data with Issues", class = "pt-1"),
      p(rule_description),
      fluidRow(
        column(
          3,
          p("Rows:", class = "badge bg-primary text-wrap text-primary fs-5"),
          p(items, class = "fs-5 fw-bold")
        ),
        column(
          3,
          p("NULL Values:", class = "badge bg-warning text-wrap text-warning fs-5"),
          p(nNA, class = "fs-5 fw-bold")
        ),
        column(
          3,
          p("Passed the Criteria:", class = "badge bg-success text-wrap text-success fs-5"),
          p(passes, class = "fs-5 fw-bold")
        ),
        column(
          3,
          p("Failed:", class = "badge bg-danger text-danger text-wrap fs-5"),
          p(fails, class = "fs-5 fw-bold")
        )
      ),
      fluidRow(column(
        12,
        align = "center",
        
        datatable(
          rows_with_issues,
          options = list(
            paging = TRUE,
            ## paginate the output
            pageLength = 15,
            ## number of rows to output for each page
            scrollX = TRUE,
            ## enable scrolling on X axis
            scrollY = TRUE,
            ## enable scrolling on Y axis
            autoWidth = TRUE,
            ## use smart column width handling
            server = FALSE,
            ## use client-side processing
            dom = 'Bfrtip'
            
          ),
          filter = 'bottom',
          ## include column filters at the bottom
          rownames = FALSE                ## don't show row numbers/names
        )
      ))
    )
  })
}

present_details_contributions <- function(budget_contributions){
  budget_contributions %>%
    fmt_currency(
      columns = c(refugee_budget, resilience_budget),
      currency = "USD",
      decimals = 0
    ) %>%
    tab_spanner(
      label = "Budget contributions amount",
      columns = c(refugee_budget, resilience_budget)
    )%>%
    tab_header(
      title = md("**SUMMARY OF THE BUDGET CONTRIBUTIONS**"),
      subtitle = "Provide key informations about each sector budget contributions for the sector lead and co-lead"
    ) %>%
    cols_label(
      planning_year ="Planning Year",
      sector = "Sector",
      donors = "Donors",
      refugee_budget ="Refugee Contributions",
      resilience_budget ="Resilience contributions",
    )  %>%
    sub_missing(
      columns = 2
    ) %>%
    summary_rows(
      groups = TRUE,
      columns = c(refugee_budget, resilience_budget),
      fns = list(
        TOTAL = ~sum(.,na.rm = TRUE)),
      formatter = fmt_currency
    ) %>%
    grand_summary_rows(
      columns = c(refugee_budget, resilience_budget),
      fns = list(
        GRAND_TOTAL = ~sum(.,na.rm = TRUE)),
      formatter = fmt_currency
    ) %>%
    tab_style_body(
      style = cell_fill(color = "red",alpha = 0.3),
      values = c(0.00)
    ) %>%
    tab_style_body(
      style = cell_fill(color = "red",alpha = 0.3),
      fn = function(x) is.na(x)
    ) %>%
    opt_horizontal_padding(scale = 3) %>%
    opt_stylize(style = 2, color = "green") %>%
    tab_options(
      row_group.background.color = "#d3a588",
      row_group.border.top.color = "#faad4f",
      row_group.border.bottom.style = "none",
      table.width = "97%",
      column_labels.background.color = "#0a6e4f"
    ) %>% opt_all_caps()
}

present_details_summary_sector <- function(sector_data_table) {
  sector_data_table %>%
    tab_spanner(
      label = "Outputs",
      columns = c(number_of_outputs, number_of_outputs_with_budget)
    ) %>%
    tab_spanner(
      label = "Indicators",
      columns = c(number_of_indicators, number_of_indicators_with_target)
    )%>%
    tab_spanner(
      label = "Requirements",
      columns = c(youth_budget_plan, refugee_budget_plan, resilience_budget_plan, budget_requirement_plan)
    ) %>%
    tab_spanner(
      label = "Contributions",
      columns = c(refugee_budget_contributions, resilience_budget_contributions, budget_requirement_contributions)
    ) %>%
    fmt_currency(
      columns = c(youth_budget_plan, refugee_budget_plan, resilience_budget_plan, budget_requirement_plan,refugee_budget_contributions, resilience_budget_contributions, budget_requirement_contributions),
      currency = "USD",
      decimals = 0
    ) %>%
    tab_header(
      title = md("**SUMMARY BY SECTOR**"),
      subtitle = "Provide key informations about each sector covered"
    ) %>%
    cols_label(
      planning_year = "Planning Year",
      youth_budget_plan = "Youth",
      refugee_budget_plan = "Refugee",
      resilience_budget_plan = "Resilience",
      budget_requirement_plan = "Total",
      refugee_budget_contributions = "Refugee",
      resilience_budget_contributions = "Resilience",
      budget_requirement_contributions = "Total",
      refugee_budget_contributions = "Refugee",
      number_of_outputs = html("# of Outputs"),
      number_of_outputs_with_budget = "Outputs With Budget",
      number_of_indicators =  html("# of Indicators"),
      number_of_indicators_with_target =  html("Indicators With Target")
    )%>%
    tab_style_body(
      style = cell_fill(color = "red",alpha = 0.3),
      values = c(0.00)
    ) %>%
    opt_horizontal_padding(scale = 1) %>%
    opt_stylize(style = 2, color = "green") %>%
    tab_options(
      row_group.background.color = "#d3a588",
      row_group.border.top.color = "#faad4f",
      row_group.border.bottom.style = "none",
      table.width = "97%",
      column_labels.background.color = "#0a6e4f"
    )%>%
    opt_all_caps()
}



present_details_summary_budget_requirement <- function(budget_requirement_data_table) {
  budget_requirement_data_table %>%
    fmt_currency(
      columns = c(youth_budget, budget_requirement, refugee_budget, resilience_budget),
      currency = "USD",
      decimals = 0
    ) %>%
    tab_spanner(
      label = "Budget requirements amount",
      columns = c(youth_budget, budget_requirement, refugee_budget, resilience_budget)
    )%>%
    tab_header(
      title = md("**SUMMARY OF THE BUDGET REQUIREMENTS**"),
      subtitle = "Provide key informations about each sector budget requirements for the sector lead and co-lead"
    ) %>%
    cols_label(
      planning_year ="Planning Year",
      output = "Output",
      budget_requirement ="Budget Requirement",
      youth_budget ="Youth Budget",
      refugee_budget ="Refugee Budget",
      resilience_budget ="Resilience Budget",
    ) %>%
    grand_summary_rows (
      columns = c(refugee_budget, resilience_budget,budget_requirement,youth_budget),
      fns = list(
        G_TOT = ~sum(.,na.rm = TRUE)),
      formatter = fmt_currency
    ) %>%
    tab_style_body(
      style = cell_fill(color = "red",alpha = 0.3),
      values = c(0.00)
    ) %>%
    opt_horizontal_padding(scale = 3) %>%
    opt_stylize(style = 2, color = "green") %>%
    tab_options(
      row_group.background.color = "#d3a588",
      row_group.border.top.color = "#faad4f",
      row_group.border.bottom.style = "none",
      table.width = "97%",
      column_labels.background.color = "#0a6e4f"
    )%>%
    opt_all_caps()
}

change_consideration_py <-
  function(data_to_update,
           to_consider_value,
           formId_value,
           recordIdColumn = "id") {
    filter_to_do <- "Yes"
    if (to_consider_value == "Yes")
      filter_to_do <- "No"
    if (nrow(data_to_update) > 0)
    {
      data_to_update <-
        data_to_update %>% filter(to_consider ==  filter_to_do) %>% mutate(to_consider = to_consider_value) %>% select(id, to_consider) %>% filter(!is.null(id))
      if (nrow(data_to_update)  >  0)
      {
        glimpse(data_to_update)
        importTable(formId = formId_value,
                    data = data_to_update,
                    recordIdColumn = "id")
      }
      
    }
  }



# Define UI for application that draws
ui <- fluidPage(
  useWaiter(),
  waiterOnBusy(html = tagList(spin_fading_circles(),
                              "Processing ...")),
  theme =  my_theme,
  # authentication module
  auth_ui(
    id = "auth",
    # add image on top ?
    tags_top =
      tags$div(
        tags$h4("3RP Data Collection - Agency Planning Tool", style = "align:center")
      ),
    # add information on bottom ?
    tags_bottom = tags$div(
      tags$p(
        paste0(
          "Access to the application is restricted to organizations accepted as 3RP ",p_year," appealing partners. If you do not have the access credentials, please submit a request for access to the UNHCR point of contact."
        )
      ),
      tags$p(
        "For any question, please contact ",
        tags$a(
          href = "mailto:matayo@unhcr.org",
          target = "_top",
          "3RP Data Collection - Agency Planning Tool Manager"
        )
      )
    ),
    # set language ?
    lan = use_language("en")
  ),
  
  useShinyjs(),
  
  # Application title
  titlePanel("3RP Data Collection - Agency Planning Tool"),
  helpText(
    paste0(
      "The Agency Planning Tool is designed to assist sector coordinators to collect inputs from participating agencies on their intended activities and budgets for ",
      p_year,
      "."
    )
  ),
  br(),
  fluidRow(column(
    12, tags$style("#project-grid {
                      display: grid;
                      grid-template-columns: 200px 1fr;
                      grid-gap: 10px;
                      align-items: center;
                      }
                   "),
    div(id = "project-grid",
        div(img(src = '01full.png', alt = "3RP Logo", width = '200px')),
        div(fluidRow(
          column(
            12,
            align = "left",
            htmlOutput("status_of_server")
          ),
          column(
            6,
            align = "left",
            htmlOutput("status_of_sector_lead")
          ),
          column(
            3,
            align = "left",
            actionButton("select_sector_to_see", label = "Select Sector",class="btn btn-primary text-bg-primary")
          ),
          column(
            3,
            align = "left",
            actionButton("access_to_is_data", label = "Access Inter Sector Review",class="btn btn-primary text-bg-primary")
          )
        )))

  )),
  fluidRow(column(
    12,
    tabsetPanel(
      type = "pills",
      
      id = "navigation_tab",
      tabPanel(
        "Instructions",
        id = "instructions_tab",
        h1("Introduction"),
        HTML("<p style=‘text-align:justify’>This platform efficiently handles the 3RP&#39;s information by supplying all the necessary components for the 3RP&#39;s data coordination.</p>
<p style=‘text-align:justify’>Data coordination is the process of organizing, managing, and distributing data so that it is accurate, consistent, and useable. Effective data coordination is necessary for ensuring that data is accessible, dependable, and valuable and for allowing businesses to maximize their data assets.</p>
<p style=‘text-align:justify’>This platform provides various crucial data coordination pieces, including:</p>
<ul>
	<li style=‘text-align: justify;’><strong>Data governance</strong>: This involves establishing policies, procedures, and standards for managing data, including who has access to it and how it can be used. The governance is driven by 3RP guidance and is integrated into the tool steps to implement.</li>
	<li style=‘text-align: justify;’><strong>Data management</strong>: This includes storing, organizing, and protecting data, as well as ensuring that it is accurate, consistent, and up to date. An embedded data quality module ensures that the appropriate stakeholders know and address the data quality issues promptly.</li>
	<li style=‘text-align: justify;’><strong>Data sharing</strong>: It involves making data available to authorized users within an organization or beyond, including using data portals or other tools. The system allows downloading the data in excel format and sharing it with other stakeholders.</li>
	<li style=‘text-align: justify;’><strong>Data integration</strong>: This involves combining data from different sources, systems, or formats to create a single, cohesive view of the data. All the data produced in the system will be directly registered in Activity Info, and that can facilitate the follow-up and ensure that the data is well integrated with existing systems in place for planning and monitoring.</li>
</ul>
<p style=‘text-align:justify’>Data coordination is essential because it helps organizations use and leverage their data assets effectively and make informed, data-driven decisions. Organization, IM, Sector Leads, and Inter-Sectoral coordination groups can access the same dataset during the modifications using this unique tool.</p>"),
        h1("Instructions on Organization Planning"),
        HTML(
          paste0(
            "<p style=‘text-align:justify’>The<strong> Agency Planning Tool</strong> is designed to assist sector coordinators in collecting inputs from participating agencies on their intended activities and budgets for ",
            p_year,
            ". The participating agencies should include agencies that are supposed to get their budget directly from donors and not from implementing agencies. The implementing agencies will be reflected through activities if required or during the monitoring phase.</p>
<p style=‘text-align:justify’><strong><em>Note: </em></strong><em>This tool is not intended for publication; it should be seen as a tool for coordinators to collect inputs. </em></p>
<p style=‘text-align:justify’>Once sector coordinators have received inputs from the participating agencies, they should confirm with UNHCR IM Unit for compilation. The UNHCR IM Unit will share with them the <strong>Sector Response Matrix </strong>containing the compiled version of the sector response. The <strong>Sector Response Matrix </strong>will then be published as the Sector&#39;s log frame in the 3RP country chapter. It will also be submitted to the 3RP Regional Technical Committee to extract relevant data for the region-wide aggregation of indicators, targets, and budgets.</p>
<p style=‘text-align:justify’>For further information on setting <strong>indicators</strong>, refer to <a href='https://www.3rpsyriacrisis.org/guidancenotes/'><strong>Guidance Note</strong></a>, and for technical assistance with these matrices, don&#39;t hesitate to contact <strong>Roshna Abdulrahman</strong> at <strong><a href=‘mailto:abdulrar@unhcr.org’>abdulrar@unhcr.org</a></strong>, <strong>Mahmood Saeed </strong><a href=‘mailto:saeedma@unhcr.org’ style=‘color:#0563c1; text-decoration:underline’><strong>saeedma@unhcr.org</strong></a> keeping in copy<strong> Stanyslas Matayo <a href=‘mailto:matayo@unhcr.org’>matayo@unhcr.org</a></strong>.</p>"
          )
        ),
        h2("Step by step Organization Planning"),
        fluidRow(column(
          12,
          align = "center",
          DiagrammeR::grVizOutput('step_by_step', width = "100%", height = "1200px")
        )),
        br(),
        HTML(
          paste0("<p style=‘text-align:justify’><strong>The following application steps are required to develop the organization&#39;s strategy for the 3RP.</strong> To ensure the effectiveness of the procedure, it would be impetuous to follow the steps as described here.</p>
<p style=‘text-align:justify’><strong>Select and Update Sectors:&nbsp;</strong>Choose sectors that include the duties your organization will do in ",p_year,"</p>
<p style=‘text-align:justify’><strong>Activate Multi-Year Funds:&nbsp;</strong>If your Multi-Year Funds are used in ",p_year,", Activate the option <strong><em>&quot;Do you already have Multi-Years donor funds in your organization for ",p_year,"&quot;</em></strong>.</p>
<p style=‘text-align:justify’><strong>Select and Update Multi-Year Fund Donors List:&nbsp;</strong>Update the List of Multi-Year Donors who have already contributed to the ",p_year," budget.</p>
<p style=‘text-align:justify’><strong>Activate Carry-Over Fund Donors:&nbsp;</strong>If you have to carry over funds from the previous year into ",p_year,", Activate the option&quot;<strong><em> Will the cash given to your organization in previous years by donors be carried over into ",p_year,"&quot;.</em></strong></p>
<p style=‘text-align:justify’><strong>Select and Update Carry Over Fund Donors List:&nbsp;</strong>Update the List of Carry Over Fund Donors who have already contributed to the ",p_year," budget.</p>
<p style=‘text-align:justify’><strong>Save to Activity Info Budget Plan:&nbsp;</strong>Save the Budget Plan, which consists of Budget Requirements and current contributions totaling $O, to Activity Info for a future update.</p>
<p style=‘text-align:justify’><strong>Update Budget Requirements:&nbsp;</strong>Update the Data in the Table of Budget Requirements that maintains your Budgetary Requirements in <strong>USD</strong>. Update the Refugee Budget, the Resilience Budget, and the Youth Budget for Budget Requirements; the system will automatically compute the Total Budget Requirement by adding the Refugee Budget and the Resilience Budget. If the Output is irrelevant in the list of given outputs, please deactivate it by changing the Active column from Yes to No; the Active column is a Dropdown field, and you may pick No.</p>
<p style=‘text-align:justify’><strong>Update Existing Contributions:&nbsp;</strong>Update the Data in the Table <strong>Existing Contributions to</strong> keep your Existing contributions from Donors in USD by donor, sector, and contribution type up-to-date. If an Existing contribution in the provided list is irrelevant, please deactivate it by changing the Active column from Yes to No; the Active column is a dropdown field, and you may choose No. The system will automatically calculate the Total Existing Contributions by adding the Refugee and Resilience contributions.</p>
<p style=‘text-align:justify’><strong>Download in Excel format Budget Plan:&nbsp;</strong>The Budget requirements and Existing contributions can be downloaded in Excel by pressing this button.</p>
<p style=‘text-align:justify’><strong>Save to Activity Info Budget Plan:&nbsp;</strong>Save the modified Budget Plan to Activity Info, which includes Budget Requirements and existing contributions. The system will generate and update the list of Monitoring Plan Indicators to reflect only those With Budgeted Output.</p>
<p style=‘text-align:justify’><strong>Update Indicators Targets:&nbsp;</strong>Update the Data in the Table Indicators Targets to keep your Indicators Target up-to-date. If an Indicator in the provided list is irrelevant, please deactivate it by changing the Active column from Yes to No; the Active column is a dropdown field, and you may choose No.</p>
<p style=‘text-align:justify’><strong>Save to Activity Info Monitoring Plan:&nbsp;</strong>Save the modified Indicators Targets to Activity Info.</p>
<p style=‘text-align:justify’><strong>Download in Excel format Monitoring Plan:&nbsp;</strong>Download in Excel format Indicators Targets.</p>
"
        )),
        h2("Instructions on Focal point selection"),
        fluidRow(column(
          12, tags$style("#project-grid-mailchmp {
                      display: grid;
                      grid-template-columns: 100px 1fr;
                      grid-gap: 10px;
                      align-items: center;
                      }
                   "),
          div(id = "project-grid-mailchmp",
              div(img(src = 'https://us5.admin.mailchimp.com/release/e/assets/freddieIcon-6OWFHM2D.svg', alt = "MailChimp Logo", width = '100px')),
              div(a(href="http://eepurl.com/hLhblf", "For Registration with MailChimp Click here!", target="_blank")))
          
        )),
        HTML(
          "<p style=‘text-align:justify’>The form intends to collect the necessary information for registering, classifying, and identifying 3RP contacts according to their roles and responsibilities. Below is a list of mandatory fields that users must complete to submit the form:</p>
<ul>
	<li><strong>Email Address: </strong>An email address is a key identifier for the Focal point in the list that doesn&#39;t accept any duplication; once the user has been signed in with a specific email address, they are not allowed to sign-up again with the same email.</li>
	<li><strong>First Name: </strong>The first name of the focal point.</li>
	<li><strong>Last Name: </strong>The last name of the focal point.</li>
	<li><strong>Phone Number: </strong>The focal point provides the phone number for direct contact and requests.</li>
	<li><strong>Governorate: </strong>The Iraqi Governorate where the focal point is currently employed<strong>.</strong></li>
	<li><strong>Organization: </strong>The organization for which the focal point is now employed<strong>.</strong></li>
	<li><strong>Coordination Role</strong>: This is the function of the coordination&#39;s current focal point, which includes Sector Lead, Sector Co-Lead, Member (to receive updates on 3RP and engage in the execution of the response, appealing partner, or donor), Sector IM, and ISWG Member. The coordination role illustrates the organization of the working group and who is responsible for which significant coordination activity.</li>
	<li><strong>Sector:</strong> The chosen sectors must be related to an investment made by your organization as appealing or implementing partners.</li>
	<li><strong>Data Collection Focal Point: </strong>As part of implementing the 3RP response, it is essential to specify the role of registered individuals in submitting data about the coordination of the 3RP response.<strong> </strong>In this section, the focal point will choose what area they will be an active and focal point for and what will be the main functionality(s); the options are as below:
	<ul>
		<li><strong>Funding Requirements:</strong> Setting Funding requirements during the planning phase.</li>
		<li><strong>Funding Update:</strong> Focal point for submitting the Funding Updates received during the year to UNHCR focal point.</li>
		<li><strong>Funding Reviewer:</strong> The reviewer focal point of the submitted funds in each organization; the person has read-only access to funding and can suggest some updates to people in charge of providing requirements and updates.</li>
		<li><strong>Indicators Selection:</strong> Focal point for selecting the indicators under each sector during the planning phase.</li>
		<li><strong>Indicators Update:</strong> Focal point for updating the indicators during each organization&#39;s monitoring process throughout the year. (ActivityInfo Reporting focal point).</li>
		<li><strong>Indicators Reviewer:</strong> Internal and external focal points for reviewing the updated indicators in each period can be reached for the submitted figures validation process. the person has read-only access to indicators and can suggest some updates to those in charge of providing planning and monitoring data related to indicators.</li>
	</ul>
	</li>
</ul>

<blockquote>
<p><strong>Note:</strong> Any subscriber who is no longer functioning in the Iraq operation can easily unsubscribe through a click under each email sent through MailChimp.</p>
</blockquote>"
        )
      ),
      tabPanel(
        "Planning Requirements",
        id = "planning_tab",
        h1("Planning Requirements"),
        h2("Select Sectors covered by your organization"),
        HTML(
          paste0(
            "<p style=‘text-align:justify’>The sectors selected will help us to define <em><strong>budget requirements</strong></em> by output and a coherent <em><strong>sectoral response log frame</strong></em>, including indicators with targets for <strong>",
            p_year,
            " </strong>at the objective and output level, including, where possible, <em><strong>targets against regionally coherent strategic direction indicators</strong></em>.</p>
             <p style='text-align:justify'><strong>PLEASE ENSURE TO SELECT ALL APPLICABLE SECTORS FOR YOUR ORGANIZATION.</strong></p>"
          )
        ),
        multiInput(
          inputId = "list_sectors",
          label = "",
          width = "100%",
          choices = c("Waiting for the DB"),
          options = list(
            enable_search = TRUE,
            non_selected_header = "Choose Sectors:",
            selected_header = "Sectors selected:"
          )
        ),
        h2("Select Multi-Years Donors"),
        materialSwitch(
          inputId = "multi_years_funds",
          label = paste0(
            "Do you already have Multi-Years donor funds in your organization for ",
            p_year,
            "?"
          ),
          value = FALSE,
          status = "success",
          width = "100%",
          right = TRUE
        ),
        hidden(
          multiInput(
            inputId = "list_my_donors",
            label = "",
            width = "100%",
            choices = c("Waiting for the DB"),
            options = list(
              enable_search = TRUE,
              non_selected_header = "Choose Multi-Years Donors:",
              selected_header = "Multi-Years Donors selected:"
            )
          )
        ),
        h2("Select Carry-Over Donors"),
        materialSwitch(
          inputId = "carry_over_funds",
          label = paste0(
            "Will the cash given to your organization in previous years by donors be carried over into ",
            p_year,
            "?"
          ),
          value = FALSE,
          status = "success",
          width = "100%",
          right = TRUE
        ),
        hidden(
          multiInput(
            inputId = "list_co_donors",
            label = "",
            width = "100%",
            choices = c("Waiting for the DB"),
            options = list(
              enable_search = TRUE,
              non_selected_header = "Choose Carried-Over Donors:",
              selected_header = "Carried-Over Donors selected:"
            )
          )
        ),
        
        fluidRow(column(
          6,
          offset = 6,
          align = "right",
          actionButton("to_db_apply", label = "Send to Activity Info",class="btn btn-primary text-bg-primary")
        )),
        h2("Provide Budget Requirements of your organization"),
        HTML(
          paste0(
            "<p style=‘text-align:justify’><strong>Budgets </strong>for appealing partners (UN agencies and NGOs) are listed by Sector and Component (<em><strong>refugees </strong></em>and <em><strong>resilience</strong></em>), and requirements against No Lost Generation deliverables for ",
            p_year,
            " are noted.</p>"
          )
        ),
        h3("Budget Requirements per Output"),
        HTML(
          "All financial demands, including those already covered by donations and those not covered by the organization, are covered by the Total Budget. Within the framework of the 3RP, requirements are gathered at the output level for each sector in which the company participates."
        ),
        br(),
        rHandsontableOutput('tbl_budget_requirement'),
        tags$style('#tbl_budget_requirement * { word-wrap: break-word}'),
        br(),
        h3("Existing Financial Contributions"),
        HTML(
          "In terms of the organization's budget, some of the organization's demands are already met by donations covering many years or by contributions from prior years that will be carried over to the next year. We shall record these contributions and identify the type of contributions and donors."
        ),
        br(),
        rHandsontableOutput('tbl_existing_budget'),
        tags$style('#tbl_existing_budget * { word-wrap: break-word}'),
        br(),
        fluidRow(
          column(
            4,
            offset = 4,
            align = "right",
            downloadButton("to_xlsx_download", label = "Download")
          ),
          column(
            4,
            align = "right",
            actionButton("to_db_outputs", label = "Send to Activity Info",class="btn btn-primary text-bg-primary")
          )
        )
        
      ),
      tabPanel(
        "Indicators Targets",
        id = "indicators_tab",
        h1("Indicators Targets"),
        HTML("<p style=‘text-align:justify’>Visible indicators are those for which outputs have a budgetary requirement; consequently, they may provide a target that can be achieved within the following year.</p>
<p style=‘text-align:justify’><strong>Targets of the indicator </strong>should be feasible within the <em><strong>time</strong></em>, <strong><em>capacity</em></strong>, and <em><strong>resources </strong></em>available. Output indicators in this section will be specific to each Output and developed to measure the Output&rsquo;s level of <em><strong>achievement/completion</strong></em>. Resilience Output indicators should also concretely capture the means of the relevant Output.</p>
<p style=‘text-align:justify’><em><strong>If you are not accountable for all sectors or outputs, please update the indicators within your authority; otherwise, poor planning may ensue.</strong></em></p>"
        ),
        br(),
        HTML("<p style='text-align:justify'><strong>Indicator targets</strong> are essential for monitoring since they give the organization a target to aim towards. By establishing a target for an indicator, organizations may assess their progress over time and determine whether they are advancing toward their aspirations. This may assist them in deciding if their efforts are having the intended effect and motivate them to continue working towards the goal. A goal may help organizations prioritize their work and deploy resources more efficiently. For instance, if an organization has limited resources, it may prioritize indications with the most significant potential effect or indicators furthest from its goal. This may assist them in maximizing the effectiveness of their efforts.</p>"),
        rHandsontableOutput('tbl_indicators'),
        tags$style('#tbl_indicators * {word-wrap: break-word}'),
        br(),
        fluidRow(
          column(
            4,
            offset = 4,
            align = "right",
            downloadButton("to_xlsx_download_indicator", label = "Download")
          ),
          column(
            4,
            align = "right",
            actionButton("to_db_indicators", label = "Send to Activity Info",class="btn btn-primary text-bg-primary")
          )
        ),
        br()
      ),
      tabPanel(
        "Data Quality",
        h1("Data quality Problems"),
        actionButton("to_refresh_quality", label = "Refresh Criterias",class="btn btn-success btn-lg text-bg-success"),
        HTML("<p style='text-align:justify'>Accurate and trustworthy facts are vital for <strong>making informed judgments,</strong> which results in <strong>better decision-making</strong>. By <strong>monitoring data quality</strong> regularly, your organization can guarantee that they have access to the <strong>high-quality data necessary for making better choices</strong>. In that optic and to ensure that you can track data quality over time to guarantee its accuracy and consistency, we have added a set of criteria that your data should fulfill to find and fix mistakes and inconsistencies, improving the overall correctness of the data. The criteria listed here are the minimum data governance rules and processes, enhancing data management and control for the <strong>3RP Planning</strong>.</p>"),
        h2("Budget Requirements"),
        uiOutput("budget_data_quality"),
        uiOutput("budget_comparaison_data_quality"),
        h2("Budget Existing Contributions"),
        uiOutput("contributions_data_quality"),
        h2("Indicators"),
        uiOutput("indicators_data_quality")
      ),
      tabPanel(
        "Data Summary",
        h1("Data Summary"),
        actionButton("to_refresh_dashboard", label = "Refresh Data",class="btn btn-success btn-lg text-bg-success"),
        h2("Key Figures Summary"),
        uiOutput("key_figures_summary"),
        h2("Requirements"),
        uiOutput("key_figures_budget_requirements"),
        h2("Contributions"),
        uiOutput("key_figures_budget_contributions"),
        h2("Sector Summary"),
        gt_output("sector_summary_table"),
        fluidRow(
          column(
            4,
            offset = 8,
            align = "right",
            downloadButton("to_xlsx_download_sector_summary", label = "Download")
          )
        )
       
      ),
        tabPanel(
          "Sector Lead Review",
          id = "sector_lead_review_tab",
          value = "sector_lead_review_tab",
          h1("Sector Lead Review"),
          HTML("<p style='text-align:justify'><strong>Sector leads</strong> should review and validate the data collected by the organization under their responsibilities. Reviewing the data is <strong>a crucial phase</strong> in the data analysis process. It entails evaluating the data thoroughly to verify that it is <strong>correct</strong>, <strong>comprehensive</strong>, and <strong>legitimate</strong>. This is crucial because erroneous or insufficient data might result in faulty conclusions and poor decision-making. In addition to identifying any faults or inaccuracies in the data, such as missing numbers or outliers, data review may also guarantee that the data is prepared for future analysis. In addition, data assessment may enhance the reliability and credibility of the analysis&#39;s findings by assuring the data&#39;s high quality. A comprehensive data review is vital for assuring the integrity and dependability of the data and the analytical outcomes.</p>"),
          actionButton("to_refresh_sector_lead", label = "Refresh Data",class="btn btn-success btn-lg text-bg-success"),
          div(class ="alert alert-danger mt-n1",
              h4("Changing organization data!",class="alert-heading"),
              p("It is not suggested to update an organization's data using this interface, since doing so may result in data inconsistencies with those already known to the organization that submitted them. Please inform the organization's focal point for any needed modifications, so they can implement them at their level inside the application."),
              hr(),
              p("In case, you want to update information here, please contact the organization focal point.",class="mb-0")),
          h2("Key Figures Summary"),
          uiOutput("key_figures_summary_sec"),
          h2("Requirements"),
          uiOutput("key_figures_budget_requirements_sec"),
          h2("Contributions"),
          uiOutput("key_figures_budget_contributions_sec"),
          h2("Budget Requirements Review"),
          gt_output('tbl_budget_requirement_sec'),
          h2("Budget Contributions Review"),
          gt_output('tbl_existing_budget_sec'),
          h2("Indicator Review"),
          gt_output('tbl_indicators_sec'),
          br(),
          fluidRow(
            column(
              4,
              offset = 8,
              align = "right",
              downloadButton("to_xlsx_download_review", label = "Download")
            )
          )
          
        ), tabPanel(
          "Inter-Sector Lead Review",
          id = "inter_sector_lead_review_tab",
          value = "inter_sector_lead_review_tab",
          h1("Inter-Sector Lead Review"),
          h2("Key Figures Summary"),
          uiOutput("key_figures_summary_all"),
          h2("Requirements"),
          uiOutput("key_figures_budget_requirements_all"),
          h2("Contributions"),
          uiOutput("key_figures_budget_contributions_all"),
          h2("Sector Summary"),
          gt_output("sector_summary_table_all"),
          h2("Budget Requirements Review"),
          gt_output('tbl_budget_requirement_sec_all'),
          h2("Budget Contributions Review"),
          gt_output('tbl_existing_budget_sec_all'),
          h2("Indicator Review"),
          gt_output('tbl_indicators_sec_all'),
          br(),
          fluidRow(
            column(
              4,
              offset = 8,
              align = "right",
              downloadButton("to_xlsx_download_review_all", label = "Download")
            )
          )
        )
      
    )
  )),
  br(),
  br(),
  spsGoTop("default", icon = icon("arrow-up"))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Create the object with no values
  values <- reactiveValues()
  
  values$refresh_budget <- TRUE
  values$refresh_contributions <- TRUE
  values$refresh_indicators <- TRUE
  values$refresh_sectors <- TRUE
  values$refresh_selection_py <- TRUE
  values$refresh_dashboard <- TRUE
  values$refresh_quality <- TRUE
  values$refresh_sector_lead <- TRUE
  values$refresh_is_sector_lead <- TRUE
  
  # authentication module
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  values$number_of_sector <- 0
  values$number_of_outputs <- 0
  values$number_of_indicators <- 0
  values$budget_requirements <- 0
  values$budget_contributions <- 0
  values$existing_donors_contributions <- 0
  values$refugee_budget_requirements <- 0
  values$refugee_budget_contributions <- 0
  values$resilience_budget_requirements <- 0
  values$resilience_budget_contributions <- 0
  values$multi_year_contributions <- 0
  values$carry_over_budget_contributions <- 0
  values$multi_year_donors_contributions <- 0
  values$carry_over_donors_budget_contributions <- 0
  
  output$key_figures_summary <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    fluidRow(
      summaryBox(
        "# Of Sectors",
      values$number_of_sector,
      icon = "fas fa-clipboard-list",
      width = 2,
      style = "info"
    ),summaryBox(
      "# Of Outputs",
      values$number_of_outputs,
      icon = "fas fa-clipboard-list",
      width = 2,
      style = "info"
    ),summaryBox(
      "# Of Indicators",
      values$number_of_indicators,
      icon = "fas fa-clipboard-list",
      width = 2,
      style = "info"
    ),summaryBox(
      "# of Existing Donors",
      values$existing_donors_contributions,
      icon = "fas fa-clipboard-list",
      width = 2,
      style = "info"
    ),summaryBox(
      "# of Multi-Years Donors",
      values$multi_year_donors_contributions,
      icon = "fas fa-clipboard-list",
      width = 2,
      style = "success"
    ),summaryBox(
      "# of Carry-Over Donors",
      values$carry_over_donors_budget_contributions,
      icon = "fas fa-clipboard-list",
      width = 2,
      style = "success"
    )
    )
    
  })
  
  output$key_figures_budget_requirements <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    fluidRow(
      summaryBox(
      "Budget Requirements",
      values$budget_requirements,
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "info"
    ),summaryBox(
      "Refugee",
      values$refugee_budget_requirements,
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "success"
    ),  summaryBox(
      "Resilience",
      values$resilience_budget_requirements,
     
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "success"
    )
    )
  })

  
  output$key_figures_budget_contributions <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    fluidRow(
      summaryBox("Existing Contributions",
               values$budget_contributions,
               
               icon = "fas fa-dollar-sign",
               width = 2,
               style = "info"
    ),summaryBox("Refugee",
      values$refugee_budget_contributions,
      
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "success"
    ),summaryBox("Resilience",
      values$resilience_budget_contributions,
      
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "success"
    ), summaryBox(
      "Multi-Years",
      values$multi_year_contributions,
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "success"
    ),summaryBox(
      "Carry-Over",
      values$carry_over_budget_contributions,
      icon = "fas fa-dollar-sign",
      width = 2,
      style = "success"
    )
    )
    
  })

  values$number_of_sector_sec <- 0
  values$number_of_outputs_sec <- 0
  values$number_of_indicators_sec <- 0
  values$budget_requirements_sec <- 0
  values$budget_contributions_sec <- 0
  values$existing_donors_contributions_sec <- 0
  values$refugee_budget_requirements_sec <- 0
  values$refugee_budget_contributions_sec <- 0
  values$resilience_budget_requirements_sec <- 0
  values$resilience_budget_contributions_sec <- 0
  values$multi_year_contributions_sec <- 0
  values$carry_over_budget_contributions_sec <- 0
  values$multi_year_donors_contributions_sec <- 0
  values$carry_over_donors_budget_contributions_sec <- 0
  
  output$key_figures_summary_sec <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    fluidRow(
      summaryBox(
        "# Of Sectors",
        values$number_of_sector_sec,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# Of Outputs",
        values$number_of_outputs_sec,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# Of Indicators",
        values$number_of_indicators_sec,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# of Existing Donors",
        values$existing_donors_contributions_sec,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# of Multi-Years Donors",
        values$multi_year_donors_contributions_sec,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "success"
      ),summaryBox(
        "# of Carry-Over Donors",
        values$carry_over_donors_budget_contributions_sec,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "success"
      )
    )
    
  })
  
  output$key_figures_budget_requirements_sec <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    fluidRow(
      summaryBox(
        "Budget Requirements",
        values$budget_requirements_sec,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "info"
      ),summaryBox(
        "Refugee",
        values$refugee_budget_requirements_sec,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      ),  summaryBox(
        "Resilience",
        values$resilience_budget_requirements_sec,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      )
    )
  })
  
  
  output$key_figures_budget_contributions_sec <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    fluidRow(
      summaryBox("Existing Contributions",
                 values$budget_contributions_sec,
                 icon = "fas fa-dollar-sign",
                 width = 2,
                 style = "info"
      ),summaryBox("Refugee",
                   values$refugee_budget_contributions_sec,
                   
                   icon = "fas fa-dollar-sign",
                   width = 2,
                   style = "success"
      ),summaryBox("Resilience",
                   values$resilience_budget_contributions_sec,
                   
                   icon = "fas fa-dollar-sign",
                   width = 2,
                   style = "success"
      ), summaryBox(
        "Multi-Years",
        values$multi_year_contributions_sec,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      ),summaryBox(
        "Carry-Over",
        values$carry_over_budget_contributions_sec,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      )
    )
  })
  
  values$number_of_sector_all <- 0
  values$number_of_outputs_all <- 0
  values$number_of_indicators_all <- 0
  values$budget_requirements_all <- 0
  values$budget_contributions_all <- 0
  values$existing_donors_contributions_all <- 0
  values$refugee_budget_requirements_all <- 0
  values$refugee_budget_contributions_all <- 0
  values$resilience_budget_requirements_all <- 0
  values$resilience_budget_contributions_all <- 0
  values$multi_year_contributions_all <- 0
  values$carry_over_budget_contributions_all <- 0
  values$multi_year_donors_contributions_all <- 0
  values$carry_over_donors_budget_contributions_all <- 0
  
  output$key_figures_summary_all <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    fluidRow(
      summaryBox(
        "# Of Sectors",
        values$number_of_sector_all,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# Of Outputs",
        values$number_of_outputs_all,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# Of Indicators",
        values$number_of_indicators_all,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# of Existing Donors",
        values$existing_donors_contributions_all,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "info"
      ),summaryBox(
        "# of Multi-Years Donors",
        values$multi_year_donors_contributions_all,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "success"
      ),summaryBox(
        "# of Carry-Over Donors",
        values$carry_over_donors_budget_contributions_all,
        icon = "fas fa-clipboard-list",
        width = 2,
        style = "success"
      )
    )
    
  })
  
  output$key_figures_budget_requirements_all <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    fluidRow(
      summaryBox(
        "Budget Requirements",
        values$budget_requirements_all,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "info"
      ),summaryBox(
        "Refugee",
        values$refugee_budget_requirements_all,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      ),  summaryBox(
        "Resilience",
        values$resilience_budget_requirements_all,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      )
    )
  })
  
  
  output$key_figures_budget_contributions_all <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    fluidRow(
      summaryBox("Existing Contributions",
                 values$budget_contributions_all,
                 icon = "fas fa-dollar-sign",
                 width = 2,
                 style = "info"
      ),summaryBox("Refugee",
                   values$refugee_budget_contributions_all,
                   
                   icon = "fas fa-dollar-sign",
                   width = 2,
                   style = "success"
      ),summaryBox("Resilience",
                   values$resilience_budget_contributions_all,
                   
                   icon = "fas fa-dollar-sign",
                   width = 2,
                   style = "success"
      ), summaryBox(
        "Multi-Years",
        values$multi_year_contributions_all,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      ),summaryBox(
        "Carry-Over",
        values$carry_over_budget_contributions_all,
        icon = "fas fa-dollar-sign",
        width = 2,
        style = "success"
      )
    )
  })
  
  output$status_of_server <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    values$org_name <-
      organization_first %>% filter(code == auth$user)
    HTML(
      paste0(
        "<p style='text-align:justify' class='align-middle'><span style='font-size:18px'><strong>Organization Authentified: <u><em>",
        values$org_name$code_name,
        "</em></u></strong></span></p>"
      )
    )
  })
  
  output$status_of_sector_lead <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    text_sl <- paste0(
      "<p style='text-align:justify' class='align-middle'><span style='font-size:18px'><strong>Sector of the Sector Lead: <u><em>NOT SELECTED</em></u></strong></span></p>"
    )
  if(nrow(values$main_sector) == 1) text_sl <- paste0(
        "<p style='text-align:justify' class='align-middle'><span style='font-size:18px'><strong>Sector of the Sector Lead: <u><em>",
        values$main_sector$code_name,
        "</em></u></strong></span></p>"
      )
    HTML(
      text_sl
    )
  })
  
  
  output$step_by_step <- DiagrammeR::renderGrViz({
    grViz(
      "digraph {

graph [layout = dot, label='STEP BY STEP ORGANIZATION PLANNING PROCESS',fontname='Lato,Arial,sans-serif']

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = '#333333', fontcolor= White]
data0 [label = 'Organization']
data1 [label = 'Sectors', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
data2 [label = 'Multi-Year Donors', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
data3 [label = 'Carry-Overs Funds Donors', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
data4 [label = 'Sectorial Focal Points', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
process [label =  'Generate Budget Plan']
data5 [label =  'Budget Requirements per Output', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
data6 [label =  'Existing Funds per Sector', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
process_br [label =  'Generate Monitoring Plan']
data7 [label = 'Indicator Target per Budgeted Output', shape = folder, fillcolor = '#0a6e4f', fontcolor= White]
process_com1 [label = 'Compilation by IM Unit']
process_com2 [label = 'Review by Sector Leads']
process_com3 [label = 'Validation by the Coordination']
results [label= 'Results' , shape = folder, fillcolor = '#f26147']
data_c1 [label =  'Data Summary', shape = folder, fillcolor = '#f26147', fontcolor= White]
data_c2 [label =  'Data Quality', shape = folder, fillcolor = '#f26147', fontcolor= White]
data_c3 [label =  'Data Integration', shape = folder, fillcolor = '#f26147', fontcolor= White]

subgraph cluster4 {
rank = same; 
subgraph cluster0 {
rank = same; data1; data2; data3; label = 'Data Requirements';
};
subgraph cluster1 {
rank = same; data5; data6; label = 'Budgeting Data';
};
process;
label = 'Planning Requirements';
}

subgraph cluster2 {
rank = same; process_com1; process_com2; process_com3;label = 'Consolidation and Validation';
}
subgraph cluster3 {
rank = same; data7;label = 'Indicators Targets';
}
subgraph cluster5 {
rank = same; data_c1; data_c2; data_c3; label = 'Data Capabilities';
}

# edge definitions with the node IDs
data0 -> data1 -> {data2 data3}  -> process -> {data5 data6}

data5 -> process_br
process_com1 -> process_com2 -> process_com3 -> results
data0 -> data4
process_br -> data7
{data7 data5 data6 data4} -> process_com1
data_c1 -> data_c2 -> data_c3 [style=invis];
data0 -> data_c1
data_c3 -> results
}")
  })
  
  indicator_form_id <- "ccjnpjylaxmcyfq4r"
  indicator_reference_column <- "is_3rp"
  
  regional_indicator_form_id <- "ca2yfb3lap3qd5lp"
  budget_planning_form_id <- "cwjs0inlb0qh70z5"
  budget_py_planning_form_id <- "cd3dthxlb4xutv8a"
  indicator_planning_form_id <- "c3jsofplb96e4x51g"
  
  sector_form_id <- "croqgk6lal21i30nyl"
  sector_reference_column <- "three_rp"
  
  organization_form_id <- "cajkf0zlal21i30nyy"
  organization_reference_column <- "appealing"
  
  output_form_id <- "cd6g7molal21i30nyt"
  output_reference_column <- "is_3rp"
  
  session$onSessionEnded(function()
  {
    print('hello, the session has ended')
  })
  
  steps_table_id <- "ckh1tqxlal21i30ny5"
  
  disable("to_db_apply")
  print("steps")
  steps <- queryTable(
    steps_table_id,
    "Activity" = "cg690tnl264eh90d.cfs6pvcl2643ezn5",
    "Quarter" = "ct0nl2zl264h881m",
    "Coordination Focal point" = "c2nw0ujlb0he6zcl",
    "Email Coordination Focal Point" = "ctffrmclb0hekc3m",
    "Technical Focal point" = "c88shnvlb0hf0kun",
    "Email Technical Focal Point" = "c5bbxzblb0hfh0go",
    "Access Token" = "cdjht3flb65abhv2",
    "Deadline" = "css9o0ll264gtj0g",
    "Effective Date" = "cupr3qyl264gcayf",
    "Active" = "cfu2xadlb0dg85md",
    truncate.strings = FALSE
  ) %>% janitor::clean_names()
  
  
  
  preselection <-
    steps %>% filter(activity == "07 - Preselection - Planning")
  planning <-
    steps %>% filter(activity == "09 - Organization Planning")
  
  values$preselection <- preselection
  values$preselection_closed <-
    nrow(preselection %>% filter(active == "No")) > 0
  
  values$planning <- planning
  values$planning_open <-
    nrow(planning %>% filter(active == "Yes")) > 0
  
  
  select_yes_no <- c("Yes", "No")
  
  observe({
    req(auth$result)  # <---- dependency on authentication result
    useSweetAlert()
    if (!values$planning_open)
    {
      planning <- values$planning
      name_coord_fp <- planning$coordination_focal_point[1]
      mail_coord_fp <- planning$email_coordination_focal_point[1]
      name_tech_fp <- planning$technical_focal_point[1]
      mail_tech_fp <- planning$email_technical_focal_point[1]
      
      sendSweetAlert(
        title = "Organization Planning step closed",
        text = HTML(
          paste0(
            "<p style=‘text-align:justify’>The application&#39;s <strong>organization planning</strong> phase is temporarily unavailable; don&#39;t hesitate to contact the focal points to amend the application&#39;s data; in the interim, you have read-only access to existing data.</p>
<p style=‘text-align:justify’><strong>Coordination Focal Point</strong> :&nbsp;",
            name_coord_fp,
            " ",
            mail_coord_fp,
            "</p>
<p style=‘text-align:justify’><strong>Technical Focal Point</strong>:&nbsp;",
            name_tech_fp,
            " ",
            mail_tech_fp,
            "</p>"
          )
        ) ,
        type = "warning",
        html = TRUE,
        btn_colors = "#0a6e4f"
      )
      disable("to_db_outputs")
      disable("to_db_indicators")
    }
    
  })
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function(sectors) {
    modalDialog(
      title = "ACCESS THE SECTOR YOU ARE LEADING OR CO-LEADING",
      HTML("<p style='text-align:justify'>You may get the details for your sector by choosing the sector for which you serve as the lead. To get access to the data of your whole sector through the lead sector tab, please choose the sector for which you serve as the lead and provide its access token. If you do not have the access token, contact the UNHCR IM unit.</p>"),
      selectInput("sectors_to_monitor", "Select your Sector:",
                  sectors),
      passwordInput("sectors_to_monitor_token", "Provide the Sector Token:"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_sectors_to_monitor", "OK")
      )
    )
  }
  
  observeEvent(
    input$ok_sectors_to_monitor,
    {
      sec_t <-  isolate(values$sector_for_this_lead_sec)
      req(auth$result)  # <---- dependency on authentication result
      c <- sec_t %>% filter(code_name == isolate(input$sectors_to_monitor) & access_token == isolate(input$sectors_to_monitor_token))
      if(nrow(c) == 1) {
        values$main_sector <- c
      } else {
        values$main_sector <- sec_t[0, ]
      }
      removeModal()
    })
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal_IS <- function() {
    modalDialog(
      title = "ACCESS THE INTER-SECTOR DATA",
      HTML("<p style='text-align:justify'>To access the full dataset through the inter sector lead tab, please provide its access token. If you do not have the access token, contact the UNHCR IM unit. You may get the details for all datasets if you are part of the Intersectoral working group and deal with the coordination.</p>"),
      passwordInput("inter_sectors_to_monitor_token", "Provide the Inter-Sector Token:"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_inter_sectors_to_monitor", "OK")
      )
    )
  }
  
  observeEvent(
    input$ok_inter_sectors_to_monitor,
    {
      req(auth$result)  # <---- dependency on authentication result
      c <- isolate(values$planning) %>% filter(access_token == isolate(input$inter_sectors_to_monitor_token))
      if(nrow(c) == 1) {
        values$is_sector_lead <- c
      }
      removeModal()
    })

  sector_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    req(values$org_name)
    print("sector_refresh")
   s <- queryTable(
      sector_form_id,
      "id" = "_id",
      "3RP" = "cedz9l4ksa4g63ct",
      "Code Name" = "cnv3sosksa3n9gh3",
      "Code" = "cr5u0neksa5cnnz1i",
      "Effective Sector" = "cit2ts4ksa4hk5y17",
      "Lead" = "cwxsxuvlbmmtv1h2.c5ytxhyks7s2ygds",
      "Access Token" = "cmma2o1lbpepm4g2",
      "Co-Lead" = "cr5wcpvlbmmunhl3.c5ytxhyks7s2ygds",
      "Active" = "cn2o33wksa4kntm1g",
      truncate.strings = FALSE
    ) %>% janitor::clean_names() %>% filter(active == "Yes" &
                                              effective_sector == "Yes" &
                                              x3rp == "Yes") %>% mutate(code_name_1 = code_name) %>% remove_rownames %>% column_to_rownames(var = "code_name_1")
  
   values$sector_for_this_lead_sec <- s %>% filter(co_lead == values$org_name$code_name | lead == values$org_name$code_name)
   
   values$sector_lead <- nrow(values$sector_for_this_lead_sec) > 0
  
  if(values$sector_lead) {
    showModal(dataModal(unique(values$sector_for_this_lead_sec$code_name)))
   } else {
    disable("select_sector_to_see") 
    values$main_sector <- s[0,]
   } 
  s
    })
  
  output_table_id <-  "cd6g7molal21i30nyt"
  
  output_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    print("output_refresh")
    queryTable(
      output_table_id,
      "id" = "_id",
      "Sector" = "caiwiqzl20fr2xjb.cnv3sosksa3n9gh3",
      "Output" = "cz9wy11l1um54a69",
      "3RP" = "c5fgcodlaqk259gy",
      filter = paste0("c5fgcodlaqk259gy == 'Yes'"),
      truncate.strings = FALSE
    ) %>% janitor::clean_names() %>% mutate(output_1 = output) %>% remove_rownames %>% column_to_rownames(var = "output_1")
    
  })
  
  
  
  indicator_references_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    print("indicator_references_refresh")
     queryTable(
      "ch02jx3lb95bphk7",
      "id" = "_id",
      "Sector" = "c14zmpqlb95c3sq9.cnv3sosksa3n9gh3",
      "Output" = "cm85z1olb9tb5nic.cz9wy11l1um54a69",
      "Component" = "c6yezdolb95c3sqb",
      "Indicator" = "cvdgro6lb95c3srm",
      "Regional" = "cftf7d5lb95c3srv",
      "Active" = "cp3lrjwlb9m876x9",
      filter = "cp3lrjwlb9m876x9 == 'Yes'",
      truncate.strings = FALSE
    ) %>% janitor::clean_names() %>% mutate(indicator_1 = indicator) %>% remove_rownames %>% column_to_rownames(var = "indicator_1")
    
  })
  
  organization_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    print("organization_refresh")
    queryTable(
      organization_form_id,
      "id" = "_id",
      "Appealing" = "cd6kakml733j7ga7",
      "Code Name" = "c5ytxhyks7s2ygds",
      "Acronym" = "cmguuq9ks78tw006",
      "Code" = "c2lrasyks78sxm75",
      "Alternative Name" = "cyq1e07ks7rrfxv5",
      "Type" = "cg097b7ks794p6m10.cvbf9fbks7uv2msy",
      "Implementer" = "cltmkhoks7rwer8q",
      "Donor" = "cc6kkk8ks7rtbj7i",
      "Active" = "comzfppks7rsqhvb",
      truncate.strings = FALSE
    ) %>% janitor::clean_names() %>% filter(active == "Yes") %>% mutate(code_name_1 = code_name)  %>% remove_rownames %>% column_to_rownames(var = "code_name_1")
    
  })
  
  indicator_target_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_indicators
    print("indicator_target_refresh")
    queryTable(
      indicator_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cbd3bxalb98bxd42a",
      "Organization" = "chstnf1lb96fok91i.c5ytxhyks7s2ygds",
      "Sector" = "cdew5q9lb97cfa826.cnv3sosksa3n9gh3",
      "Output" = "c4eqiztlb9w5vgw2.cz9wy11l1um54a69",
      "Indicator" = "cxrmk41lb96eurt1h.cvdgro6lb95c3srm",
      "Indicator Target" = "cbi0x2slb96gb711j",
      "To consider" = "c91mi18lbb6jnxcc",
      "Active" = "cmtq82flb96gnff1p",
      filter = paste0(
        "chstnf1lb96fok91i.c5ytxhyks7s2ygds == '",
        values$org_name$code_name,
        "'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
    
  })
  
  get_indicator_target_refresh_sec_all <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_is_sector_lead
    print("get_indicator_target_refresh_sec_all")
    queryTable(
      indicator_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cbd3bxalb98bxd42a",
      "Organization" = "chstnf1lb96fok91i.c5ytxhyks7s2ygds",
      "Sector" = "cdew5q9lb97cfa826.cnv3sosksa3n9gh3",
      "Output" = "c4eqiztlb9w5vgw2.cz9wy11l1um54a69",
      "Indicator" = "cxrmk41lb96eurt1h.cvdgro6lb95c3srm",
      "Indicator Target" = "cbi0x2slb96gb711j",
      "To consider" = "c91mi18lbb6jnxcc",
      "Active" = "cmtq82flb96gnff1p",
      filter = paste0(
        "cmtq82flb96gnff1p == 'Yes' && c91mi18lbb6jnxcc == 'Yes'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
  })
  
  indicator_target_refresh_sec <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    values$refresh_sector_lead
    isolate(get_indicator_target_refresh_sec_all()) %>% filter(sector %in% unique(values$main_sector$code_name))
    
  })
  
  planning_py_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_budget
    values$refresh_contributions
    sectors_to_consider <-
      isolate(sector_refresh()) %>% filter(x3rp == "Yes")
    sectors_to_consider_v2 <-
      isolate(planning_refresh()) %>% filter(to_consider == "Yes")
    req(values$org_name)
    print("planning_py_refresh")
    t <- queryTable(
      budget_py_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cf71s78lb4xv6emb",
      "Year" = "cnh9nk2lb4xyyrwo",
      "Sector" = "c4gyj5rlb4xv6enc.cnv3sosksa3n9gh3",
      "Organization" = "cnghew1lb4xv6eod.c5ytxhyks7s2ygds",
      "Budget Type" = "cfu6nkwlb4xv6eof",
      "Donors" = "c6ou2qtlb725oq02.c5ytxhyks7s2ygds",
      "Refugee Budget" = "c8t5vualb4xv6epj",
      "Resilience Budget" = "c3kqsvslb4xv6epk",
      "To consider" = "cdwwtlklbb6ixwc9",
      "Active" = "crbzchylb4xv6epl",
      filter = paste0(
        "cnghew1lb4xv6eod.c5ytxhyks7s2ygds == '",
        values$org_name$code_name,
        "'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
    
    to_inactivate <-
      t %>% filter(
        !sector %in% unique(sectors_to_consider$code_name) |
          !sector %in% unique(sectors_to_consider_v2$sector) | is.na(donors) | donors ==""
      )
    if (nrow(to_inactivate) > 0)
    {
      to_inactivate <-
        to_inactivate %>% filter(to_consider ==  "Yes") %>% mutate(to_consider = "No")
      to_inactivate <-
        to_inactivate %>% select(id, to_consider) %>% filter(!is.null(id))
      if (nrow(to_inactivate) >  0)
      {
        glimpse(to_inactivate)
        importTable(formId = budget_py_planning_form_id,
                    data = to_inactivate,
                    recordIdColumn = "id")
      }
    }
    print("planning_py_refresh")
    queryTable(
      budget_py_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cf71s78lb4xv6emb",
      "Year" = "cnh9nk2lb4xyyrwo",
      "Sector" = "c4gyj5rlb4xv6enc.cnv3sosksa3n9gh3",
      "Organization" = "cnghew1lb4xv6eod.c5ytxhyks7s2ygds",
      "Budget Type" = "cfu6nkwlb4xv6eof",
      "Donors" = "c6ou2qtlb725oq02.c5ytxhyks7s2ygds",
      "Refugee Budget" = "c8t5vualb4xv6epj",
      "Resilience Budget" = "c3kqsvslb4xv6epk",
      "To consider" = "cdwwtlklbb6ixwc9",
      "Active" = "crbzchylb4xv6epl",
      filter = paste0(
        "cnghew1lb4xv6eod.c5ytxhyks7s2ygds == '",
        values$org_name$code_name,
        "'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
    
  })
  
  get_planning_py_refresh_sec_all <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_is_sector_lead
    print("get_planning_py_refresh_sec_all")
    queryTable(
      budget_py_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cf71s78lb4xv6emb",
      "Year" = "cnh9nk2lb4xyyrwo",
      "Sector" = "c4gyj5rlb4xv6enc.cnv3sosksa3n9gh3",
      "Organization" = "cnghew1lb4xv6eod.c5ytxhyks7s2ygds",
      "Budget Type" = "cfu6nkwlb4xv6eof",
      "Donors" = "c6ou2qtlb725oq02.c5ytxhyks7s2ygds",
      "Refugee Budget" = "c8t5vualb4xv6epj",
      "Resilience Budget" = "c3kqsvslb4xv6epk",
      "To consider" = "cdwwtlklbb6ixwc9",
      "Active" = "crbzchylb4xv6epl",
      filter = paste0(
        "crbzchylb4xv6epl == 'Yes' && cdwwtlklbb6ixwc9 == 'Yes'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
  })
  
  planning_py_refresh_sec <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    values$refresh_sector_lead
    isolate(get_planning_py_refresh_sec_all()) %>% filter(sector %in% unique(values$main_sector$code_name))
  })
  
  summary_per_sector <- reactive({
    b_summary <-
      isolate(planning_refresh()) %>% filter(active == "Yes" &
                                               to_consider == "Yes") %>% group_by(planning_year, sector, organization) %>% summarize(
                                                 youth_budget_plan = sum(youth_budget),
                                                 refugee_budget = sum(refugee_budget),
                                                 resilience_budget = sum(resilience_budget),
                                                 budget_requirement_plan = sum(budget_requirement),
                                                 number_of_outputs = n_distinct(output),
                                                 number_of_outputs_with_budget = length(unique(output[budget_requirement >0]))
                                               )
    
    c_summary <-
      isolate(planning_py_refresh())    %>% filter(active == "Yes" &
                                                     to_consider == "Yes") %>% group_by(planning_year, sector, organization) %>% summarize(
                                                       refugee_budget = sum(refugee_budget),
                                                       resilience_budget = sum(resilience_budget)
                                                     )
    i_summary <- isolate(indicator_target_refresh())  %>% filter(active == "Yes" &
                               to_consider == "Yes") %>% group_by(planning_year, sector, organization) %>% summarize(
                                 number_of_indicators = n_distinct(indicator),
                                 number_of_indicators_with_target = length(unique(indicator[indicator_target >0]))
                               )
    
    summarized_version <-
      b_summary %>% left_join(
        c_summary,
        by = c("planning_year", "sector", "organization"),
        suffix = c("_plan", "_contributions")
      ) %>% mutate_if(is.numeric, ~  replace_na(., 0)) %>% mutate(budget_requirement_contributions = refugee_budget_contributions + resilience_budget_contributions)
    
    summarized_version <-
      summarized_version %>% left_join(
        i_summary,
        by = c("planning_year", "sector", "organization")) %>% mutate_if(is.numeric, ~  replace_na(., 0))
    
    summarized_version
  })
  
  summary_per_sector_all <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_is_sector_lead
    b_summary <-
      isolate(get_planning_refresh_sec_all()) %>% group_by(planning_year, sector, organization) %>% summarize(
                                                 youth_budget_plan = sum(youth_budget),
                                                 refugee_budget = sum(refugee_budget),
                                                 resilience_budget = sum(resilience_budget),
                                                 budget_requirement_plan = sum(budget_requirement),
                                                 number_of_outputs = n_distinct(output),
                                                 number_of_outputs_with_budget = length(unique(output[budget_requirement >0]))
                                               )
    
    c_summary <-
      isolate(get_planning_py_refresh_sec_all()) %>% group_by(planning_year, sector, organization) %>% summarize(
                                                       refugee_budget = sum(refugee_budget),
                                                       resilience_budget = sum(resilience_budget)
                                                     )
    i_summary <- isolate(get_indicator_target_refresh_sec_all()) %>% group_by(planning_year, sector, organization) %>% summarize(
                                                                     number_of_indicators = n_distinct(indicator),
                                                                     number_of_indicators_with_target = length(unique(indicator[indicator_target >0]))
                                                                   )
    
    summarized_version <-
      b_summary %>% left_join(
        c_summary,
        by = c("planning_year", "sector", "organization"),
        suffix = c("_plan", "_contributions")
      ) %>% mutate_if(is.numeric, ~  replace_na(., 0)) %>% mutate(budget_requirement_contributions = refugee_budget_contributions + resilience_budget_contributions)
    
    summarized_version <-
      summarized_version %>% left_join(
        i_summary,
        by = c("planning_year", "sector", "organization")) %>% mutate_if(is.numeric, ~  replace_na(., 0))
    
    summarized_version
  })
  
  planning_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    req(values$org_name)
    values$refresh_budget
    values$refresh_contributions
    sectors_to_consider <-
      isolate(sector_refresh()) %>% filter(x3rp == "Yes")
    indicator_to_consider <- isolate(indicator_references_refresh())
    print("planning_refresh")
    print(values$org_name$code_name)
    value_filter <-  paste0(
      "cpa1ggxlb0qhy477.c5ytxhyks7s2ygds == '",
      values$org_name$code_name,"'"
    )
    print(value_filter)
    
    t <- queryTable(
      budget_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cp0hc5slb3iu2g59",
      "Sector" = "cdnqtiqlb0qhy466.cnv3sosksa3n9gh3",
      "Organization" = "cpa1ggxlb0qhy477.c5ytxhyks7s2ygds",
      "Output" = "cq2e3oflb0qhy488.cz9wy11l1um54a69",
      "Budget Requirement" = "coj6cv0lb0qhy48e",
      "Youth Budget" = "cibq5rulb0qhy48f",
      "Refugee Budget" = "celmri7lb0qhy48g",
      "Resilience Budget" = "cpy8fv9lb0qhy48h",
      "To consider" = "c920wa8lbb6gebw7",
      "Active" = "cv6oh41lb3d38yw7",
      filter = value_filter,
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
   
    print("end_planning_refresh")
    
    to_inactivate <-
      t %>% filter(
        !output %in% unique(indicator_to_consider$output) |
          !sector %in% unique(sectors_to_consider$code_name)
      )
    to_inactivate <-
      to_inactivate %>% filter(to_consider ==  "Yes") %>% mutate(to_consider = "No")
    to_inactivate <-
      to_inactivate %>% select(id, to_consider) %>% filter(!is.null(id))
    if (nrow(to_inactivate) > 0)
    {
      glimpse(to_inactivate)
      importTable(formId = budget_planning_form_id,
                  data = to_inactivate,
                  recordIdColumn = "id")
      
    }
    print("planning_refresh")
    queryTable(
      budget_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cp0hc5slb3iu2g59",
      "Sector" = "cdnqtiqlb0qhy466.cnv3sosksa3n9gh3",
      "Organization" = "cpa1ggxlb0qhy477.c5ytxhyks7s2ygds",
      "Output" = "cq2e3oflb0qhy488.cz9wy11l1um54a69",
      "Budget Requirement" = "coj6cv0lb0qhy48e",
      "Youth Budget" = "cibq5rulb0qhy48f",
      "Refugee Budget" = "celmri7lb0qhy48g",
      "Resilience Budget" = "cpy8fv9lb0qhy48h",
      "To consider" = "c920wa8lbb6gebw7",
      "Active" = "cv6oh41lb3d38yw7",
      filter = paste0(
        "cpa1ggxlb0qhy477.c5ytxhyks7s2ygds == '",
        values$org_name$code_name,
        "'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
  })
  
  get_planning_refresh_sec_all <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_is_sector_lead
    print("get_planning_refresh_sec_all")
    queryTable(
      budget_planning_form_id,
      "id" = "_id",
      "Planning Year" = "cp0hc5slb3iu2g59",
      "Sector" = "cdnqtiqlb0qhy466.cnv3sosksa3n9gh3",
      "Organization" = "cpa1ggxlb0qhy477.c5ytxhyks7s2ygds",
      "Output" = "cq2e3oflb0qhy488.cz9wy11l1um54a69",
      "Budget Requirement" = "coj6cv0lb0qhy48e",
      "Youth Budget" = "cibq5rulb0qhy48f",
      "Refugee Budget" = "celmri7lb0qhy48g",
      "Resilience Budget" = "cpy8fv9lb0qhy48h",
      "To consider" = "c920wa8lbb6gebw7",
      "Active" = "cv6oh41lb3d38yw7",
      filter = paste0(
        "cv6oh41lb3d38yw7 == 'Yes' && c920wa8lbb6gebw7 == 'Yes'"
      ),
      truncate.strings = FALSE
    ) %>% janitor::clean_names()
  })
  
  planning_refresh_sec <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    values$refresh_sector_lead
    isolate(get_planning_refresh_sec_all()) %>% filter(sector %in% unique(values$main_sector$code_name))
  })
  
  budget_py_refresh <- reactive({
    req(auth$result)  # <---- dependency on authentication result
    req(values$org_name)
    values$refresh_selection_py
    
    sector_data <- isolate(sector_refresh())
    donors <-
      isolate(organization_refresh()) %>% filter(donor == "Yes")
    planning_py <-isolate(planning_py_refresh()) %>% filter(to_consider == "Yes")
    
    data_planning_my <-
      planning_py %>% filter(budget_type == "Multi-Year" &
                               to_consider == "Yes")
    data_planning_co <-
      planning_py %>% filter(budget_type == "Carry-Over" &
                               to_consider == "Yes")
    if(input$multi_years_funds)
    updateMultiInput(
      session = session,
      inputId = "list_my_donors",
      choices = sort(unique(donors$code_name)),
      selected = sort(unique(data_planning_my$donors))
    )
    if(input$carry_over_funds)
    updateMultiInput(
      session = session,
      inputId = "list_co_donors",
      choices = sort(unique(donors$code_name)),
      selected = sort(unique(data_planning_co$donors))
    )
  })
  
  output$budget_data_quality <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_quality
    present_mistakes(
      isolate(planning_refresh()) %>% filter(active == "Yes" &
                                      to_consider == "Yes"),
      rules_budget
    )
  })
  
  output$sector_summary_table_all <- render_gt({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    dt <- summary_per_sector_all() %>% gt()
    dt <- present_details_summary_sector(dt) %>%
      summary_rows(
        groups = TRUE,
        columns = c(youth_budget_plan, refugee_budget_plan, resilience_budget_plan, budget_requirement_plan,refugee_budget_contributions, resilience_budget_contributions, budget_requirement_contributions),
        fns = list(
          TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_currency
      ) %>%
      summary_rows(
        groups = TRUE,
        columns = c(number_of_outputs, number_of_outputs_with_budget,number_of_indicators, number_of_indicators_with_target),
        fns = list(
          TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_integer
      ) %>%
      grand_summary_rows (
        columns = c(youth_budget_plan, refugee_budget_plan, resilience_budget_plan, budget_requirement_plan,refugee_budget_contributions, resilience_budget_contributions, budget_requirement_contributions),
        fns = list(
          G_TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_currency
      ) %>%
      grand_summary_rows (
        columns = c(number_of_outputs, number_of_outputs_with_budget,number_of_indicators, number_of_indicators_with_target),
        fns = list(
          G_TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_integer
      )
  })
  
  output$sector_summary_table <- render_gt({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_dashboard
    dt <- summary_per_sector() %>% gt()
    dt <- present_details_summary_sector(dt) %>%
      grand_summary_rows (
        columns = c(youth_budget_plan, refugee_budget_plan, resilience_budget_plan, budget_requirement_plan,refugee_budget_contributions, resilience_budget_contributions, budget_requirement_contributions),
        fns = list(
          G_TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_currency
      ) %>%
      grand_summary_rows (
        columns = c(number_of_outputs, number_of_outputs_with_budget,number_of_indicators, number_of_indicators_with_target),
        fns = list(
          G_TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_number
      )
})
  
  output$budget_comparaison_data_quality <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_quality
    
    summarized_version <-
    isolate(summary_per_sector())  
    
    glimpse(summarized_version)
    
    present_mistakes(summarized_version, rules_comparaison)
    
  })
  
  
  
  output$contributions_data_quality <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_quality
     present_mistakes(
      planning_py_refresh() %>% filter(active == "Yes" &
                                         to_consider == "Yes"),
      rules_contributions
    )
    
  })
  
  output$indicators_data_quality <- renderUI({
    req(auth$result)  # <---- dependency on authentication result
    values$refresh_quality
      present_mistakes(
      indicator_target_refresh() %>% filter(active == "Yes" &
                                              to_consider == "Yes"),
      rules_target
    )
  })
  
  
  observe({
    req(auth$result)  # <---- dependency on authentication result
    req(values$org_name)
    # To be reviewed
    values$refresh_dashboard
    donors <-
      isolate(organization_refresh()) %>% filter(donor == "Yes")
    planning_data <-
      isolate(planning_refresh()) %>% filter(to_consider == "Yes")
    
    sector_data <- isolate(sector_refresh())
    planning_py <-
     isolate(planning_py_refresh()) %>% filter(to_consider == "Yes")
    
    data_planning_my <-
      planning_py %>% filter(budget_type == "Multi-Year")
    data_planning_co <-
      planning_py %>% filter(budget_type == "Carry-Over")
    
    planning_data_ <- planning_data %>% filter(active == "Yes")
    planning_py_ <- planning_py %>% filter(active == "Yes")
    data_planning_my_ <-
      data_planning_my %>% filter(active == "Yes")
    data_planning_co_ <-
      data_planning_co %>% filter(active == "Yes")
    indicator_target_ <-
      isolate(indicator_target_refresh()) %>% filter(to_consider == "Yes" &
                                              active == "Yes")
    
    number_of_sector <- length(unique(planning_data_$sector))
    number_of_outputs <- length(unique(planning_data_$output))
    number_of_indicators <-
      length(unique(indicator_target_$indicator))
    budget_requirements <-
      sum(
        planning_data_$refugee_budget + planning_data_$resilience_budget,
        na.rm = TRUE
      )
    budget_contributions <-
      sum(
        planning_py_$refugee_budget + planning_py_$resilience_budget,
        na.rm = TRUE
      )
    existing_donors_contributions <-
      length(unique(planning_py_$donors))
    refugee_budget_requirements <-
      sum(planning_data_$refugee_budget, na.rm = TRUE)
    refugee_budget_contributions <-
      sum(planning_py_$refugee_budget, na.rm = TRUE)
    resilience_budget_requirements <-
      sum(planning_data_$resilience_budget, na.rm = TRUE)
    resilience_budget_contributions <-
      sum(planning_py_$resilience_budget, na.rm = TRUE)
    multi_year_contributions <-
      sum(
        data_planning_my_$refugee_budget + data_planning_my_$resilience_budget,
        na.rm = TRUE
      )
    carry_over_budget_contributions <-
      sum(
        data_planning_co_$refugee_budget + data_planning_co_$resilience_budget,
        na.rm = TRUE
      )
    multi_year_donors_contributions <-
      length(unique(data_planning_my_$donors))
    carry_over_donors_budget_contributions <-
      length(unique(data_planning_co_$donors))
    
    values$number_of_sector <-
      prettyNum(number_of_sector,
                big.mark = ",",
                scientific = FALSE)
    values$number_of_outputs <-
      prettyNum(number_of_outputs,
                big.mark = ",",
                scientific = FALSE)
    values$number_of_indicators <-
      prettyNum(number_of_indicators,
                big.mark = ",",
                scientific = FALSE)
    
    values$budget_requirements <-
      formattable::currency(budget_requirements, digits = 0L)
    values$budget_contributions <-
      formattable::currency(budget_contributions, digits = 0L)
    values$existing_donors_contributions <-
      prettyNum(existing_donors_contributions,
                big.mark = ",",
                scientific = FALSE)
    values$refugee_budget_requirements <-
      formattable::currency(refugee_budget_requirements, digits = 0L)
    values$refugee_budget_contributions <-
      formattable::currency(refugee_budget_contributions, digits = 0L)
    values$resilience_budget_requirements <-
      formattable::currency(resilience_budget_requirements, digits = 0L)
    values$resilience_budget_contributions <-
      formattable::currency(resilience_budget_contributions, digits = 0L)
    values$multi_year_contributions <-
      formattable::currency(multi_year_contributions, digits = 0L)
    values$carry_over_budget_contributions <-
      formattable::currency(carry_over_budget_contributions, digits = 0L)
    
    values$multi_year_donors_contributions <-
      prettyNum(multi_year_donors_contributions,
                big.mark = ",",
                scientific = FALSE)
    values$carry_over_donors_budget_contributions <-
      prettyNum(
        carry_over_donors_budget_contributions,
        big.mark = ",",
        scientific = FALSE
      )
    
    updateMultiInput(
      session = session,
      inputId = "list_sectors",
      choices = sort(unique(sector_data$code_name)),
      selected = sort(unique(planning_data$sector))
    )
    
    updateMultiInput(
      session = session,
      inputId = "list_my_donors",
      choices = sort(unique(donors$code_name)),
      selected = sort(unique(data_planning_my$donors))
    )
    
    updateMultiInput(
      session = session,
      inputId = "list_co_donors",
      choices = sort(unique(donors$code_name)),
      selected = sort(unique(data_planning_co$donors))
    )
    
    updateMaterialSwitch(session,
                         "multi_years_funds",
                         value = nrow(data_planning_my) > 0)
    updateMaterialSwitch(session,
                         "carry_over_funds",
                         value = nrow(data_planning_co) > 0)
  })
  
  observe({
    req(auth$result)  # <---- dependency on authentication result
    req(values$main_sector)
    values$refresh_sector_lead

    planning_data <-
      isolate(planning_refresh_sec()) %>% filter(to_consider == "Yes")
    
    planning_py <-
      isolate(planning_py_refresh_sec()) %>% filter(to_consider == "Yes")
    
    data_planning_my <-
      planning_py %>% filter(budget_type == "Multi-Year")
    data_planning_co <-
      planning_py %>% filter(budget_type == "Carry-Over")
    
    planning_data_ <- planning_data %>% filter(active == "Yes")
    planning_py_ <- planning_py %>% filter(active == "Yes")
    data_planning_my_ <-
      data_planning_my %>% filter(active == "Yes")
    data_planning_co_ <-
      data_planning_co %>% filter(active == "Yes")
    indicator_target_ <-
      isolate(indicator_target_refresh_sec()) %>% filter(to_consider == "Yes" &
                                                       active == "Yes")
    
    number_of_sector <- length(unique(planning_data_$sector))
    number_of_outputs <- length(unique(planning_data_$output))
    number_of_indicators <-
      length(unique(indicator_target_$indicator))
    budget_requirements <-
      sum(
        planning_data_$refugee_budget + planning_data_$resilience_budget,
        na.rm = TRUE
      )
    budget_contributions <-
      sum(
        planning_py_$refugee_budget + planning_py_$resilience_budget,
        na.rm = TRUE
      )
    existing_donors_contributions <-
      length(unique(planning_py_$donors))
    refugee_budget_requirements <-
      sum(planning_data_$refugee_budget, na.rm = TRUE)
    refugee_budget_contributions <-
      sum(planning_py_$refugee_budget, na.rm = TRUE)
    resilience_budget_requirements <-
      sum(planning_data_$resilience_budget, na.rm = TRUE)
    resilience_budget_contributions <-
      sum(planning_py_$resilience_budget, na.rm = TRUE)
    multi_year_contributions <-
      sum(
        data_planning_my_$refugee_budget + data_planning_my_$resilience_budget,
        na.rm = TRUE
      )
    carry_over_budget_contributions <-
      sum(
        data_planning_co_$refugee_budget + data_planning_co_$resilience_budget,
        na.rm = TRUE
      )
    multi_year_donors_contributions <-
      length(unique(data_planning_my_$donors))
    carry_over_donors_budget_contributions <-
      length(unique(data_planning_co_$donors))
    
    values$number_of_sector_sec <-
      prettyNum(number_of_sector,
                big.mark = ",",
                scientific = FALSE)
    values$number_of_outputs_sec <-
      prettyNum(number_of_outputs,
                big.mark = ",",
                scientific = FALSE)
    values$number_of_indicators_sec <-
      prettyNum(number_of_indicators,
                big.mark = ",",
                scientific = FALSE)
    
    values$budget_requirements_sec <-
      formattable::currency(budget_requirements, digits = 0L)
    values$budget_contributions_sec <-
      formattable::currency(budget_contributions, digits = 0L)
    values$existing_donors_contributions_sec <-
      prettyNum(existing_donors_contributions,
                big.mark = ",",
                scientific = FALSE)
    values$refugee_budget_requirements_sec <-
      formattable::currency(refugee_budget_requirements, digits = 0L)
    values$refugee_budget_contributions_sec <-
      formattable::currency(refugee_budget_contributions, digits = 0L)
    values$resilience_budget_requirements_sec <-
      formattable::currency(resilience_budget_requirements, digits = 0L)
    values$resilience_budget_contributions_sec <-
      formattable::currency(resilience_budget_contributions, digits = 0L)
    values$multi_year_contributions_sec <-
      formattable::currency(multi_year_contributions, digits = 0L)
    values$carry_over_budget_contributions_sec <-
      formattable::currency(carry_over_budget_contributions, digits = 0L)
    
    values$multi_year_donors_contributions_sec <-
      prettyNum(multi_year_donors_contributions,
                big.mark = ",",
                scientific = FALSE)
    values$carry_over_donors_budget_contributions_sec <-
      prettyNum(
        carry_over_donors_budget_contributions,
        big.mark = ",",
        scientific = FALSE
      )
    
  })
  
  observe({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    # To close the door
    values$refresh_is_sector_lead
    
    planning_data <-
      isolate(get_planning_refresh_sec_all()) %>% filter(to_consider == "Yes")
    
    planning_py <-
      isolate(get_planning_py_refresh_sec_all()) %>% filter(to_consider == "Yes")
    
    data_planning_my <-
      planning_py %>% filter(budget_type == "Multi-Year")
    data_planning_co <-
      planning_py %>% filter(budget_type == "Carry-Over")
    
    planning_data_ <- planning_data %>% filter(active == "Yes")
    planning_py_ <- planning_py %>% filter(active == "Yes")
    data_planning_my_ <-
      data_planning_my %>% filter(active == "Yes")
    data_planning_co_ <-
      data_planning_co %>% filter(active == "Yes")
    indicator_target_ <-
      isolate(get_indicator_target_refresh_sec_all()) %>% filter(to_consider == "Yes" &
                                                           active == "Yes")
    
    number_of_sector <- length(unique(planning_data_$sector))
    number_of_outputs <- length(unique(planning_data_$output))
    number_of_indicators <-
      length(unique(indicator_target_$indicator))
    budget_requirements <-
      sum(
        planning_data_$refugee_budget + planning_data_$resilience_budget,
        na.rm = TRUE
      )
    budget_contributions <-
      sum(
        planning_py_$refugee_budget + planning_py_$resilience_budget,
        na.rm = TRUE
      )
    existing_donors_contributions <-
      length(unique(planning_py_$donors))
    refugee_budget_requirements <-
      sum(planning_data_$refugee_budget, na.rm = TRUE)
    refugee_budget_contributions <-
      sum(planning_py_$refugee_budget, na.rm = TRUE)
    resilience_budget_requirements <-
      sum(planning_data_$resilience_budget, na.rm = TRUE)
    resilience_budget_contributions <-
      sum(planning_py_$resilience_budget, na.rm = TRUE)
    multi_year_contributions <-
      sum(
        data_planning_my_$refugee_budget + data_planning_my_$resilience_budget,
        na.rm = TRUE
      )
    carry_over_budget_contributions <-
      sum(
        data_planning_co_$refugee_budget + data_planning_co_$resilience_budget,
        na.rm = TRUE
      )
    multi_year_donors_contributions <-
      length(unique(data_planning_my_$donors))
    carry_over_donors_budget_contributions <-
      length(unique(data_planning_co_$donors))
    
    values$number_of_sector_all <-
      prettyNum(number_of_sector,
                big.mark = ",",
                scientific = FALSE)
    values$number_of_outputs_all <-
      prettyNum(number_of_outputs,
                big.mark = ",",
                scientific = FALSE)
    values$number_of_indicators_all <-
      prettyNum(number_of_indicators,
                big.mark = ",",
                scientific = FALSE)
    
    values$budget_requirements_all <-
      formattable::currency(budget_requirements, digits = 0L)
    values$budget_contributions_all <-
      formattable::currency(budget_contributions, digits = 0L)
    values$existing_donors_contributions_all <-
      prettyNum(existing_donors_contributions,
                big.mark = ",",
                scientific = FALSE)
    values$refugee_budget_requirements_all <-
      formattable::currency(refugee_budget_requirements, digits = 0L)
    values$refugee_budget_contributions_all <-
      formattable::currency(refugee_budget_contributions, digits = 0L)
    values$resilience_budget_requirements_all <-
      formattable::currency(resilience_budget_requirements, digits = 0L)
    values$resilience_budget_contributions_all <-
      formattable::currency(resilience_budget_contributions, digits = 0L)
    values$multi_year_contributions_all <-
      formattable::currency(multi_year_contributions, digits = 0L)
    values$carry_over_budget_contributions_all <-
      formattable::currency(carry_over_budget_contributions, digits = 0L)
    
    values$multi_year_donors_contributions_all <-
      prettyNum(multi_year_donors_contributions,
                big.mark = ",",
                scientific = FALSE)
    values$carry_over_donors_budget_contributions_all <-
      prettyNum(
        carry_over_donors_budget_contributions,
        big.mark = ",",
        scientific = FALSE
      )
  })
  
  observeEvent(
    input$to_db_apply,
    {
      req(auth$result)  # <---- dependency on authentication result
      ask_confirmation(
        inputId = "sector_confirmation",
        title = "Organization Plan update",
        text = paste0(
          "Please confirm that the specified sectors are those for which your organization will submit an appeal for its ",
          p_year,
          " exercise."
        ),
        btn_colors = c("#333333", "#0a6e4f")
      )
    })
  
  observeEvent(
    input$to_refresh_dashboard,
    {
      req(auth$result)  # <---- dependency on authentication result
      values$refresh_dashboard <- !values$refresh_dashboard
    })
  
  observeEvent(
    input$select_sector_to_see,
    {
      if(values$sector_lead) {
        showModal(dataModal(unique(values$sector_for_this_lead_sec$code_name)))
      }
    })
  
  observeEvent(
    input$access_to_is_data,
    {
      showModal(dataModal_IS())
    })
  
  observeEvent(
    input$to_refresh_sector_lead,
    {
      req(auth$result)  # <---- dependency on authentication result
      req(values$main_sector)
      values$refresh_sector_lead <- !values$refresh_sector_lead
    })
  
  observeEvent(
    input$to_refresh_quality,
    {
      req(auth$result)  # <---- dependency on authentication result
      values$refresh_quality <- !values$refresh_quality
    })

  observeEvent(
    input$multi_years_funds,
    {
      req(auth$result)  # <---- dependency on authentication result
      planning_py <- isolate(planning_py_refresh())
      sector_to_consider <-
        isolate(planning_refresh()) %>% filter(to_consider ==  "Yes") %>% select(sector)
      sector_to_consider <- unique(sector_to_consider$sector)
      donors <-
        isolate(organization_refresh()) %>% filter(donor == "Yes")
      type_v <- "Multi-Year"
      
      if (input$multi_years_funds)
      {
        data_planning_my <-
          planning_py %>% filter(
            budget_type == type_v &
              sector %in% sector_to_consider & to_consider == "No"
          )
        change_consideration_py(
          data_to_update = data_planning_my,
          to_consider_value = "Yes",
          formId_value = budget_py_planning_form_id
        )
        show("list_my_donors")
        values$refresh_selection_py <- !values$refresh_selection_py
        values$refresh_contributions <-
          !values$refresh_contributions
      }
      else
      {
        data_planning_my <-
          planning_py %>% filter(budget_type == type_v &
                                   to_consider == "Yes")
        change_consideration_py(
          data_to_update = data_planning_my,
          to_consider_value = "No",
          formId_value = budget_py_planning_form_id
        )
        values$refresh_selection_py <- !values$refresh_selection_py
        values$refresh_contributions <-
          !values$refresh_contributions
        hide("list_my_donors")
      }
      
    })
  
  observeEvent(
    input$carry_over_funds,
    {
      req(auth$result)  # <---- dependency on authentication result
      planning_py <- isolate(planning_py_refresh())
      sector_to_consider <-
        isolate(planning_refresh()) %>% filter(to_consider ==  "Yes") %>% select(sector)
      sector_to_consider <- unique(sector_to_consider$sector)
      donors <-
        isolate(organization_refresh()) %>% filter(donor == "Yes")
      type_v <- "Carry-Over"
      if (input$carry_over_funds)
      {
        data_planning_my <-
          planning_py %>% filter(
            budget_type == type_v &
              sector %in% sector_to_consider &
              to_consider == "No"
          )
        change_consideration_py(
          data_to_update = data_planning_my,
          to_consider_value = "Yes",
          formId_value = budget_py_planning_form_id
        )
        show("list_co_donors")
        values$refresh_selection_py <- !values$refresh_selection_py
        values$refresh_contributions <-
          !values$refresh_contributions
        
      }
      
      else
      {
        data_planning_my <-
          planning_py %>% filter(budget_type == type_v &
                                   to_consider == "Yes")
        change_consideration_py(
          data_to_update = data_planning_my,
          to_consider_value = "No",
          formId_value = budget_py_planning_form_id
        )
        values$refresh_selection_py <- !values$refresh_selection_py
        values$refresh_contributions <-
          !values$refresh_contributions
        hide("list_co_donors")
      }
      
    })
  
  observeEvent(
    input$sector_confirmation,
    {
      if (isTRUE(input$sector_confirmation))
      {
        b_ex_planning <- isolate(planning_refresh())
        sector_list <-  isolate(sector_refresh())
        organization_list <- isolate(organization_refresh())
        planning_py <- isolate(planning_py_refresh())
        
        existing_sectors <- c()
        proposed_sectors <- c()
        if (!is.null(b_ex_planning$sector))
          existing_sectors <- unique(b_ex_planning$sector)
        if (!is.null(values$selected_sectors))
          proposed_sectors <- unique(values$selected_sectors)
        
        sector_to_add <-
          proposed_sectors[!proposed_sectors %in% existing_sectors]
        sector_to_add = sector_to_add[!is.na(sector_to_add)]
        
        sector_to_remove <-
          existing_sectors[!existing_sectors %in% proposed_sectors]
        sector_to_remove = sector_to_remove[!is.na(sector_to_remove)]
        
        sector_to_reconsider <-
          proposed_sectors[proposed_sectors %in% existing_sectors]
        sector_to_reconsider = sector_to_reconsider[!is.na(sector_to_reconsider)]
        
        if (length(sector_to_remove) > 0)
        {
          to_inactivate <-
            b_ex_planning %>% filter(sector %in% sector_to_remove)  %>% filter(to_consider ==
                                                                                 
                                                                                 "Yes") %>% mutate(to_consider = "No")
          to_inactivate <-
            to_inactivate %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(to_inactivate)  >  0)
          {
            glimpse(to_inactivate)
            importTable(
              formId = budget_planning_form_id,
              data = to_inactivate,
              recordIdColumn = "id"
            )
          }
          
          
        }
        
        if (length(sector_to_reconsider) > 0)
        {
          to_reconsider <-
            b_ex_planning %>% filter(sector %in% sector_to_reconsider &
                                       to_consider == "No") %>% mutate(to_consider = "Yes")
          to_reconsider <-
            to_reconsider %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(to_reconsider) > 0)
          {
            glimpse(to_reconsider)
            importTable(
              formId = budget_planning_form_id,
              data = to_reconsider,
              recordIdColumn = "id"
            )
          }
          
        }
        
        if (length(sector_to_add) > 0)
        {
          sectorLookup <- sector_list %>% filter(code_name %in% sector_to_add)
          outputLookup <-
            isolate(output_refresh()) %>% filter(sector %in% sector_to_add)
          organizationLookup <-
            organization_list %>% filter(code_name == values$org_name$code_name)
          
          budget_requirements <-
            outputLookup %>% select(-x3rp) %>% mutate(
              planning_year = p_year,
              organization = organizationLookup[values$org_name$code_name, "id"] ,
              active = "Yes",
              total_budget = 0,
              youth_budget = 0,
              refugee_budget = 0,
              resilience_budget = 0
            ) %>% rowwise() %>% mutate(id = cuid()) %>% transmute(
              sector_r = sectorLookup[sector, "id"],
              output_r = outputLookup[output, "id"],
              planning_year,
              organization ,
              active,
              total_budget,
              youth_budget,
              refugee_budget,
              resilience_budget,
              to_consider = "Yes",
              id
            )
          
          glimpse(budget_requirements)
          importTable(formId = budget_planning_form_id,
                      data = budget_requirements,
                      recordIdColumn = "id")
          
        }
        
        
        
        data_planning_my <-
          planning_py %>% filter(budget_type == "Multi-Year")
        data_planning_co <-
          planning_py %>% filter(budget_type == "Carry-Over")
        
        proposed_my_donors <- c()
        if (!is.null(values$selected_my_donors))
          proposed_my_donors <- unique(values$selected_my_donors)
        
        df_my_donors <- tibble(sector = character(),
                               donor = character())
        
        for (j in 1:length(proposed_my_donors))
        {
          for (i in 1:length(proposed_sectors))
          {
            df_my_donors <-
              df_my_donors %>% add_row(sector = proposed_sectors[i], donor = proposed_my_donors[j])
          }
        }
        
        to_inactivate_my <-
          data_planning_my %>% anti_join(df_my_donors, by = c("sector" = "sector", "donors" = "donor"))
        to_reconsider_my <-
          data_planning_my %>% semi_join(df_my_donors, by = c("sector" = "sector", "donors" = "donor"))
        to_add_my <-
          df_my_donors %>% anti_join(data_planning_my,
                                     by = c("sector" = "sector", "donor" = "donors"))
        if (nrow(to_inactivate_my) > 0)
        {
          to_inactivate_my <-
            to_inactivate_my %>% filter(to_consider ==  "Yes") %>% mutate(to_consider = "No") %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(to_inactivate_my)  >  0)
          {
            glimpse(to_inactivate_my)
            importTable(
              formId = budget_py_planning_form_id,
              data = to_inactivate_my,
              recordIdColumn = "id"
            )
          }
          
        }
        
        if (nrow(to_reconsider_my) > 0)
        {
          to_reconsider_my <-
            to_reconsider_my %>% filter(to_consider == "No") %>% mutate(to_consider = "Yes") %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(to_reconsider_my)  >  0)
          {
            glimpse(to_reconsider_my)
            importTable(
              formId = budget_py_planning_form_id,
              data = to_reconsider_my,
              recordIdColumn = "id"
            )
          }
          
        }
        
        if (nrow(to_add_my) > 0)
        {
          sectorLookup <-
            sector_list %>% filter(code_name %in% to_add_my$sector)
          organizationLookup <- organization_list
          
          budget_requirements_my <-
            to_add_my %>% mutate(
              planning_year = p_year,
              organization = organizationLookup[values$org_name$code_name, "id"] ,
              active = "Yes",
              type_budget = "Multi-Year",
              refugee_budget = 0,
              resilience_budget = 0
            ) %>% rowwise() %>% mutate(id = cuid()) %>% transmute(
              sector_r = sectorLookup[sector, "id"],
              donor = organizationLookup[donor, "id"],
              planning_year,
              organization ,
              type_budget,
              active,
              refugee_budget,
              resilience_budget,
              to_consider = "Yes",
              id
            ) %>% filter(!is.na(donor) & donor != "")
          
          glimpse(budget_requirements_my)
          if (nrow(budget_requirements_my) > 0)
          importTable(formId = budget_py_planning_form_id,
                      data = budget_requirements_my,
                      recordIdColumn = "id")
        }
        
        proposed_co_donors <- c()
        if (!is.null(values$selected_co_donors))
          proposed_co_donors <- unique(values$selected_co_donors)
        
        df_co_donors <- tibble(sector = character(),
                               donor = character())
        
        for (j in 1:length(proposed_co_donors))
        {
          for (i in 1:length(proposed_sectors))
          {
            df_co_donors <-
              df_co_donors %>% add_row(sector = proposed_sectors[i], donor = proposed_co_donors[j])
          }
        }
        
        to_inactivate_co <-
          data_planning_co %>% anti_join(df_co_donors, by = c("sector" = "sector", "donors" = "donor"))
        to_reconsider_co <-
          data_planning_co %>% semi_join(df_co_donors, by = c("sector" = "sector", "donors" = "donor"))
        to_add_co <-
          df_co_donors %>% anti_join(data_planning_co,
                                     by = c("sector" = "sector", "donor" = "donors"))
        if (nrow(to_inactivate_co) > 0)
        {
          to_inactivate_co <-
            to_inactivate_co %>% mutate(to_consider == "No") %>% select(id, to_consider) %>% filter(!is.null(id))
          glimpse(to_inactivate_co)
          importTable(formId = budget_py_planning_form_id,
                      data = to_inactivate_co,
                      recordIdColumn = "id")
        }
        if (nrow(to_reconsider_co) > 0)
        {
          to_reconsider_co <-
            to_reconsider_co %>% filter(to_consider == "No") %>% mutate(to_consider = "Yes") %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(to_reconsider_co) >  0)
          {
            glimpse(to_reconsider_co)
            importTable(
              formId = budget_py_planning_form_id,
              data = to_reconsider_co,
              recordIdColumn = "id"
            )
          }
          
        }
        
        if (nrow(to_add_co) > 0)
        {
          sectorLookup <-
            sector_list %>% filter(code_name %in% to_add_co$sector)
          organizationLookup <- organization_list
          
          glimpse(to_add_co)
          budget_requirements_co <-
            to_add_co %>% mutate(
              planning_year = p_year,
              organization = organizationLookup[values$org_name$code_name, "id"] ,
              active = "Yes",
              type_budget = "Carry-Over",
              refugee_budget = 0,
              resilience_budget = 0
            ) %>% rowwise() %>% mutate(id = cuid()) %>% transmute(
              sector_r = sectorLookup[sector, "id"],
              donor = organizationLookup[donor, "id"],
              planning_year,
              organization ,
              type_budget,
              active,
              refugee_budget,
              resilience_budget,
              to_consider = "Yes",
              id
            )%>% filter(!is.na(donor) & donor != "")
          
          glimpse(budget_requirements_co)
          if (nrow(budget_requirements_co) > 0)
          importTable(formId = budget_py_planning_form_id,
                      data = budget_requirements_co,
                      recordIdColumn = "id")
        }
        values$refresh_budget <- !values$refresh_budget
        values$refresh_contributions <-
          !values$refresh_contributions
        show_alert(
          title = "Successfully Saved Sectors and Donors",
          paste0(
            "Data was successfully saved to Activity Info. Updates will be available to other users."
          ),
          type = "success",
          btn_colors = "#0a6e4f"
        )
      }
      
    }, ignoreNULL = TRUE)
  
  
  
  observeEvent(
    input$list_sectors,
    ignoreNULL = FALSE ,
    {
      req(auth$result)  # <---- dependency on authentication result
      if (values$planning_open)
        enable("to_db_apply")
      values$selected_sectors <- input$list_sectors
      print(values$selected_sectors)
    })
  
  observeEvent(
    input$list_my_donors,
    ignoreNULL = FALSE ,
    {
      req(auth$result)  # <---- dependency on authentication result
      if (values$planning_open)
        enable("to_db_apply")
      values$selected_my_donors <- input$list_my_donors
      print(values$selected_my_donors)
      
    })
  
  observeEvent(
    input$list_co_donors,
    ignoreNULL = FALSE ,
    {
      req(auth$result)  # <---- dependency on authentication result
      if (values$planning_open)
        enable("to_db_apply")
      values$selected_co_donors <- input$list_co_donors
      print(values$selected_co_donors)
    })
  
  height_table <- 450
  width_table <- 1450
  width_key <- 160
  
  output$tbl_budget_requirement = renderRHandsontable({
    rhandsontable(
      planning_refresh() %>% filter(to_consider == "Yes") %>% remove_rownames %>% column_to_rownames(var = "id"),
      height = height_table,
      width = "100%",
      
      stretchH = "all",
      rowHeaderWidth = width_key,
      colHeaders = c(
        "Planning Year",
        "Sector",
        "Organization",
        "Output",
        "Budget Requirement",
        "Youth Budget",
        "Refugee Budget",
        "Resilience Budget",
        "To Consider",
        "Active"
      ),
      showToolbar = TRUE,
      search = TRUE
    ) %>%
      hot_col(c(2, 3, 4, 9), readOnly = TRUE) %>%
      hot_col(
        5,
        readOnly = TRUE,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
      }",
        type = "numeric",
        format = "$0,0.00"
      ) %>%
      hot_col(1,
              readOnly = TRUE,
              type = "numeric",
              format = "0") %>%
      hot_col(
        c(6, 7, 8),
        readOnly = !values$planning_open,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';
      }",
        type = "numeric",
        format = "$0,0.00"
      ) %>%
      hot_col(
        10,
        readOnly = !values$planning_open,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.TextRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';
      }",
        type = "dropdown",
        source = select_yes_no
      ) %>%
      hot_cols(
        columnSorting = TRUE,
        manualColumnResize = TRUE
        ,
        colWidths = c(80, 160, 200, 250, 140, 140, 140, 140, 80, 80)
      ) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE,
        allowReadOnly = FALSE,
        allowComments = FALSE,
        allowCustomBorders = FALSE,
        customOpts = list()
      )%>%
      hot_rows(rowHeights = NULL )
  })
  
  output$tbl_budget_requirement_sec = render_gt({
   dt <- planning_refresh_sec() %>% remove_rownames %>% column_to_rownames(var = "id") %>%
      group_by(organization) %>%
      gt() %>%
      cols_hide(columns = c(sector,to_consider,active))
      dt <- present_details_summary_budget_requirement(dt)  %>%
        summary_rows(
          groups = TRUE,
          columns = c(refugee_budget, resilience_budget,budget_requirement,youth_budget),
          fns = list(
            TOTAL = ~sum(.,na.rm = TRUE)),
          formatter = fmt_currency
        ) 
  })
  
  output$tbl_budget_requirement_sec_all = render_gt({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    dt <- get_planning_refresh_sec_all() %>% remove_rownames %>% column_to_rownames(var = "id") %>%
      group_by(organization,sector) %>%
      gt() %>%
      cols_hide(columns = c(to_consider,active))
    dt <- present_details_summary_budget_requirement(dt)  %>%
      summary_rows(
        groups = TRUE,
        columns = c(refugee_budget, resilience_budget,budget_requirement,youth_budget),
        fns = list(
          TOT = ~sum(.,na.rm = TRUE)),
        formatter = fmt_currency
      ) 
  })
  
  output$tbl_existing_budget = renderRHandsontable({
    rhandsontable(
      planning_py_refresh() %>% filter(to_consider == "Yes") %>% remove_rownames %>% column_to_rownames(var = "id"),
      height = height_table,
      width = "100%",
      stretchH = "all",
      rowHeaderWidth = width_key,
      colHeaders = c(
        "Planning Year",
        "Year",
        "Sector",
        "Organization",
        "Contribution Type",
        "Donors",
        "Refugee Contribution",
        "Resilience Contribution",
        "To Consider",
        "Active"
      ),
      showToolbar = TRUE ,
      search = TRUE
    ) %>%
      hot_col(c(3, 4, 5, 6, 9), readOnly = TRUE) %>%
      hot_col(
        readOnly = !values$planning_open,
        2,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';}",
        format = "0"
      ) %>%
      hot_col(1,
              readOnly = TRUE,
              type = "numeric",
              format = "0") %>%
      hot_col(
        readOnly = !values$planning_open,
        c(7, 8),
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';}",
        type = "numeric",
        format = "$0,0.00"
      ) %>%
      hot_col(
        readOnly = !values$planning_open,
        10,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.TextRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';}",
        type = "dropdown",
        source = select_yes_no
      ) %>%
      hot_cols(
        columnSorting = TRUE,
        manualColumnResize = TRUE
        ,
        colWidths = c(80, 80, 160, 275, 100, 275, 140, 140, 80, 80)
      ) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE,
        allowReadOnly = FALSE,
        allowComments = FALSE,
        allowCustomBorders = FALSE,
        customOpts = list()
      )%>%
      hot_rows(rowHeights = NULL )
  })
  
  output$tbl_existing_budget_sec = render_gt({
   dt <- planning_py_refresh_sec() %>% remove_rownames %>% column_to_rownames(var = "id") %>%
      mutate(org_budget_type = paste0(organization," ",budget_type)) %>%
      group_by(org_budget_type) %>%
      gt() 
    
    dt <- present_details_contributions(dt)
    
   dt %>%
      cols_hide(columns = c(sector,to_consider,active,budget_type,organization))
    
  })
  
  output$tbl_existing_budget_sec_all = render_gt({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    dt <- get_planning_py_refresh_sec_all() %>% remove_rownames %>% column_to_rownames(var = "id") %>%
      mutate(org_budget_type = paste0(organization," ",budget_type)) %>%
      group_by(org_budget_type) %>%
      gt() 
    
    dt <- present_details_contributions(dt)
    
    dt %>%
      cols_hide(columns = c(to_consider,active,budget_type,organization))
    
  })
  output$tbl_indicators = renderRHandsontable({
    rhandsontable(
      indicator_target_refresh() %>% filter(to_consider == "Yes") %>% remove_rownames %>% column_to_rownames(var = "id"),
      height = height_table,
      width = "100%",
      stretchH = "all",
      rowHeaderWidth = width_key,
      colHeaders = c(
        "Planning Year",
        "Organization",
        "Sector",
        "Output",
        "Indicator",
        "Indicator Target",
        "To Consider",
        "Active"
      ),
      showToolbar = TRUE ,
      search = TRUE
    ) %>%
      hot_col(c(1, 2, 3, 4, 5, 7), readOnly = TRUE) %>%
      hot_col(1,
              readOnly = TRUE,
              type = "numeric",
              format = "0") %>%
      hot_col(
        readOnly = !values$planning_open,
        6,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';}",
        type = "numeric",
        format = "0,0.00"
      ) %>%
      hot_col(
        readOnly = !values$planning_open,
        8,
        renderer = "function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.TextRenderer.apply(this, arguments);
              td.style.background = '#CFFFF2';}",
        type = "dropdown",
        source = select_yes_no
      ) %>%
      hot_cols(
        columnSorting = TRUE,
        manualColumnResize = TRUE
        ,
        colWidths = c(80, 235, 235, 300, 300, 100, 80, 80)
      ) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE,
        allowReadOnly = FALSE,
        allowComments = FALSE,
        allowCustomBorders = FALSE,
        customOpts = list()
      )%>%
      hot_rows(rowHeights = NULL )
  })
  
  output$tbl_indicators_sec = render_gt({
    
      indicator_target_refresh_sec() %>% remove_rownames %>% column_to_rownames(var = "id") %>%
      group_by(indicator) %>%
      gt() %>%
      cols_hide(columns = c(to_consider,active)) %>%
      tab_spanner(
        label = "INDICATOR DETAILS",
        columns = c(organization,indicator_target)
      ) %>% fmt_number(
        columns = indicator_target,
        decimals = 0
      ) %>%
      tab_header(
        title = md("**SUMMARY OF THE INDICATORS TARGET**"),
        subtitle = "Provide key informations about each indicator target for the sector lead and co-lead"
      ) %>% cols_move(
        columns = output,
        after = planning_year
      ) %>%
      summary_rows(
        groups = TRUE,
        columns = c(indicator_target),
        fns = list(
          TOTAL = ~sum(.,na.rm = TRUE),
          AVG = ~mean(.,na.rm = TRUE),
          MEDIAN = ~median(.,na.rm = TRUE),
          MIN = ~min(.,na.rm = TRUE),
          MAX = ~max(.,na.rm = TRUE),
          G_AVG = ~exp(mean(log(.[.>0])))),
        formatter = fmt_number
      ) %>%
      cols_label(
        planning_year ="Planning Year",
        sector = "Sector",
        output ="Output",
        organization ="Organization",
        indicator_target ="Indicator Target"
      ) %>%
      tab_style_body(
        style = cell_fill(color = "red",alpha = 0.3),
        values = c(0.00)
      ) %>%
      opt_horizontal_padding(scale = 3) %>%
      opt_stylize(style = 2, color = "green") %>%
      tab_options(
        row_group.background.color = "#d3a588",
        row_group.border.top.color = "#faad4f",
        row_group.border.bottom.style = "none",
        table.width = "97%",
        column_labels.background.color = "#0a6e4f"
      )%>%
      opt_all_caps()
      
  })
  
  output$tbl_indicators_sec_all = render_gt({
    req(auth$result)  # <---- dependency on authentication result
    req(values$is_sector_lead)
    values$refresh_is_sector_lead
    t <- get_indicator_target_refresh_sec_all()

    t %>% remove_rownames %>% column_to_rownames(var = "id") %>%
      group_by(indicator) %>%
      gt() %>%
      cols_hide(columns = c(to_consider,active)) %>%
      tab_spanner(
        label = "INDICATOR DETAILS",
        columns = c(organization,indicator_target)
      ) %>% fmt_number(
        columns = indicator_target,
        decimals = 0
      ) %>%
      tab_header(
        title = md("**SUMMARY OF THE INDICATORS TARGET**"),
        subtitle = "Provide key informations about each indicator target for the sector lead and co-lead"
      ) %>% cols_move(
        columns = output,
        after = sector
      ) %>% cols_move(
        columns = sector,
        after = planning_year
      ) %>%
      summary_rows(
        groups = TRUE,
        columns = c(indicator_target),
        fns = list(
          TOTAL = ~sum(.,na.rm = TRUE),
          AVG = ~mean(.,na.rm = TRUE),
          MEDIAN = ~median(.,na.rm = TRUE),
          MIN = ~min(.,na.rm = TRUE),
          MAX = ~max(.,na.rm = TRUE),
          G_AVG = ~exp(mean(log(.[.>0])))),
        formatter = fmt_number
      ) %>%
      cols_label(
        planning_year ="Planning Year",
        sector = "Sector",
        output ="Output",
        organization ="Organization",
        indicator_target ="Indicator Target"
      ) %>%
      tab_style_body(
        style = cell_fill(color = "red",alpha = 0.3),
        values = c(0.00)
      ) %>%
      opt_horizontal_padding(scale = 3) %>%
      opt_stylize(style = 2, color = "green") %>%
      tab_options(
        row_group.background.color = "#d3a588",
        row_group.border.top.color = "#faad4f",
        row_group.border.bottom.style = "none",
        table.width = "97%",
        column_labels.background.color = "#0a6e4f"
      )%>%
      opt_all_caps()
    
  })
  
  observeEvent(
    input$budget_confirmation,
    {
      if (isTRUE(input$budget_confirmation))
      {
        budget <-
          (hot_to_r(input$tbl_budget_requirement)) %>% rownames_to_column()
        glimpse(budget)
        budget <-  budget %>% rowwise() %>% transmute(
          active,
          to_consider,
          output,
          total_budget = refugee_budget + resilience_budget,
          youth_budget,
          refugee_budget,
          resilience_budget,
          id = rowname
        )
        
        target_data <- isolate(indicator_target_refresh())
        glimpse(budget)
        importTable(
          formId = budget_planning_form_id,
          data = budget %>% select(-output),
          recordIdColumn = "id"
        )
        
        proposed_outputs <-
          budget %>% filter(active == "Yes" &
                              to_consider == "Yes" &
                              total_budget >  0)
        proposed_outputs <- unique(proposed_outputs$output)
        
        existing_outputs <- c()
        
        if (!is.null(target_data$output))
          existing_outputs <- unique(target_data$output)
        
        outputs_to_add <-
          proposed_outputs[!proposed_outputs %in% existing_outputs]
        outputs_to_add = outputs_to_add[!is.na(outputs_to_add)]
        print(outputs_to_add)
        outputs_to_remove <-
          existing_outputs[!existing_outputs %in% proposed_outputs]
        outputs_to_remove = outputs_to_remove[!is.na(outputs_to_remove)]
        
        outputs_to_reconsider <-
          proposed_outputs[proposed_outputs %in% existing_outputs]
        outputs_to_reconsider = outputs_to_reconsider[!is.na(outputs_to_reconsider)]
        
        if (length(outputs_to_remove) > 0)
        {
          ind_to_inactivate <-
            target_data %>% filter(output %in% outputs_to_remove)  %>% filter(to_consider ==
                                                                                
                                                                                "Yes")  %>% mutate(to_consider = "No")
          
          ind_to_inactivate <-
            ind_to_inactivate %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(ind_to_inactivate)  >  0)
          {
            glimpse(ind_to_inactivate)
            importTable(
              formId = indicator_planning_form_id,
              data = ind_to_inactivate,
              recordIdColumn = "id"
            )
          }
          
          
        }
        
        if (length(outputs_to_reconsider) > 0)
        {
          ind_to_reconsider <-
            target_data %>% filter(output %in% outputs_to_reconsider &
                                     to_consider == "No") %>% mutate(to_consider = "Yes")
          
          ind_to_reconsider <-
            ind_to_reconsider %>% select(id, to_consider) %>% filter(!is.null(id))
          if (nrow(ind_to_reconsider)  >  0)
          {
            glimpse(ind_to_reconsider)
            importTable(
              formId = indicator_planning_form_id,
              data = ind_to_reconsider,
              recordIdColumn = "id"
            )
          }
        }
        
        if (length(outputs_to_add) > 0)
        {
          sectorLookup <-
            isolate(sector_refresh())
          outputLookup <-
            isolate(output_refresh()) %>% filter(output %in% outputs_to_add)
          indicatorLookup <-
            isolate(indicator_references_refresh()) %>% filter(output %in% outputs_to_add)
          organizationLookup <-
            isolate(organization_refresh()) %>% filter(code_name == values$org_name$code_name)
          glimpse(indicatorLookup)
          indicator_requirement <- indicatorLookup %>% mutate(
            planning_year = p_year,
            organization = organizationLookup[values$org_name$code_name, "id"] ,
            active = "Yes",
            to_consider = "Yes",
            indicator_target = 0,
            indicator = id
          ) %>% rowwise() %>% mutate(id = cuid()) %>% transmute(
            sector_op = sectorLookup[sector, "id"],
            indicator,
            planning_year,
            output = outputLookup[output, 'id'],
            organization ,
            active,
            to_consider,
            indicator_target,
            id
          )
          glimpse(indicator_requirement)
          importTable(formId = indicator_planning_form_id,
                      data = indicator_requirement,
                      recordIdColumn = "id")
        }
        
        
        existing_budget <-
          (hot_to_r(input$tbl_existing_budget)) %>% rownames_to_column()
        existing_budget <-
          existing_budget %>% rowwise() %>% transmute(
            active,
            year_of_reception = year,
            total_contributions = refugee_budget + resilience_budget,
            refugee_budget,
            resilience_budget,
            id  = rowname
          )
        glimpse(existing_budget)
        importTable(formId = budget_py_planning_form_id,
                    data = existing_budget,
                    recordIdColumn = "id")
        
        values$refresh_budget <- !values$refresh_budget
        values$refresh_contributions <-
          !values$refresh_contributions
        values$refresh_indicators <- !values$refresh_indicators
        
        show_alert(
          title = "Successfully Saved Budget Requirements and Contributions",
          paste0(
            "Data was successfully saved to Activity Info. Updates will be available to other users."
          ),
          type = "success",
          btn_colors = "#0a6e4f"
        )
      }
    })
  
  observeEvent(
    input$to_db_outputs,
    {
      req(auth$result)  # <---- dependency on authentication result
      #    disable("to_db_outputs")
      ask_confirmation(
        inputId = "budget_confirmation",
        title = "Budget Plan update",
        text = paste0(
          "Please confirm that the specified Budget Plan your organization submit correspond to your appeal for ",
          p_year,
          " exercise."
        ),
        btn_colors = c("#333333", "#0a6e4f")
      )
      
    })
  
  observeEvent(
    input$to_db_indicators,
    {
      req(auth$result)  # <---- dependency on authentication result
      disable("to_db_indicators")
      
      indicators <-
        (hot_to_r(input$tbl_indicators)) %>% rownames_to_column()
      glimpse(indicators)
      indicators <-  indicators %>% rowwise() %>% transmute(active,
                                                            indicator_target ,
                                                            id  = rowname)
      
      importTable(
        formId = indicator_planning_form_id,
        data = indicators,
        recordIdColumn = "id"
      )
      
      show_alert(
        title = "Successfully Saved Indicator Targets",
        paste0(
          "Data was successfully saved to Activity Info. Updates will be available to other users."
        ),
        type = "success",
        btn_colors = "#0a6e4f"
      )
    })
  
  output$to_xlsx_download <- downloadHandler(
    filename = function()
    {
      paste0("PLANNING-DATA",
             format(Sys.time(), "%Y-%m-%d-%h-%m-%s"),
             ".xlsx")
    },
    content = function(file)
    {
      budget <-
        (hot_to_r(input$tbl_budget_requirement)) %>% rownames_to_column()
      
      glimpse(budget)
      
      existing_budget <-
        (hot_to_r(input$tbl_existing_budget)) %>% rownames_to_column()
      
      glimpse(existing_budget)
      
      wb <- createWorkbook()
      addCreator(wb, "3RP Data Collection - Agency Planning Tool")
      addWorksheet(wb, "REQUIREMENTS")
      addWorksheet(wb, "CONTRIBUTIONS")
      writeDataTable(
        wb,
        "REQUIREMENTS",
        budget,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "REQUIREMENTS"
      )
      writeDataTable(
        wb,
        "CONTRIBUTIONS",
        existing_budget,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "CONTRIBUTIONS"
      )
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
  output$to_xlsx_download_review <- downloadHandler(
    filename = function()
    {
      paste0("SECTOR-PLANNING-DATA",
             format(Sys.time(), "%Y-%m-%d-%h-%m-%s"),
             ".xlsx")
    },
    content = function(file)
    {
      budget <-
        isolate(planning_refresh_sec())  %>% filter(active == "Yes" &
                                            to_consider == "Yes")
      
      glimpse(budget)
      
      existing_budget <-
      isolate(planning_py_refresh_sec()) %>% filter(active == "Yes" &
                                               to_consider == "Yes")
      
      glimpse(existing_budget)
      
      indicator_values <-
      isolate(indicator_target_refresh_sec()) %>% filter(active == "Yes" &
                                                     to_consider == "Yes")
      
      glimpse(indicator_values)
      
      wb <- createWorkbook()
      addCreator(wb, "3RP Data Collection - Agency Planning Tool")
      addWorksheet(wb, "SECTOR_BUDGET_REQUIREMENTS")
      addWorksheet(wb, "SECTOR_BUDGET_CONTRIBUTIONS")
      addWorksheet(wb, "SECTOR_INDICATORS")
    
      writeDataTable(
        wb,
        "SECTOR_BUDGET_REQUIREMENTS",
        budget,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "SECTOR_BUDGET_REQUIREMENTS"
      )
      writeDataTable(
        wb,
        "SECTOR_BUDGET_CONTRIBUTIONS",
        existing_budget,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "SECTOR_BUDGET_CONTRIBUTIONS"
      )
      writeDataTable(
        wb,
        "SECTOR_INDICATORS",
        indicator_values,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "SECTOR_INDICATORS"
      )
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$to_xlsx_download_review_all <- downloadHandler(
    filename = function()
    {
      paste0("ALL-PLANNING-DATA",
             format(Sys.time(), "%Y-%m-%d-%h-%m-%s"),
             ".xlsx")
    },
    content = function(file)
    {
      budget <-
       isolate(get_planning_refresh_sec_all())  %>% filter(active == "Yes" &
                                            to_consider == "Yes")
      
      glimpse(budget)
      
      existing_budget <-
       isolate(get_planning_py_refresh_sec_all())  %>% filter(active == "Yes" &
                                               to_consider == "Yes")
      
      glimpse(existing_budget)
      
      indicator_values <-
       isolate(get_indicator_target_refresh_sec_all()) %>% filter(active == "Yes" &
                                                     to_consider == "Yes")
      
      glimpse(indicator_values)
      
      wb <- createWorkbook()
      addCreator(wb, "3RP Data Collection - Agency Planning Tool")
      addWorksheet(wb, "ALL_BUDGET_REQUIREMENTS")
      addWorksheet(wb, "ALL_BUDGET_CONTRIBUTIONS")
      addWorksheet(wb, "ALL_INDICATORS")
      writeDataTable(
        wb,
        "ALL_BUDGET_REQUIREMENTS",
        budget,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "ALL_BUDGET_REQUIREMENTS"
      )
      writeDataTable(
        wb,
        "ALL_BUDGET_CONTRIBUTIONS",
        existing_budget,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "ALL_BUDGET_CONTRIBUTIONS"
      )
      writeDataTable(
        wb,
        "ALL_INDICATORS",
        indicator_values,
        rowNames = TRUE,
        keepNA = FALSE,
        tableName =  "ALL_INDICATORS"
      )
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$to_xlsx_download_sector_summary <-
    downloadHandler(
      filename = function()
      {
        paste0("SECTOR-SUMMARY-DATA",
               format(Sys.time(), "%Y-%m-%d-%h-%m-%s"),
               ".xlsx")
      },
      content = function(file)
      {
        summary_per_sector <-
          summary_per_sector()
        
        glimpse(summary_per_sector)
        
        wb <- createWorkbook()
        addCreator(wb, "3RP Data Collection - Agency Planning Tool")
        addWorksheet(wb, "SECTOR_SUMMARY")
        writeDataTable(
          wb,
          "SECTOR_SUMMARY",
          summary_per_sector,
          rowNames = TRUE,
          keepNA = FALSE,
          tableName =  "SECTOR_SUMMARY"
        )
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  
  output$to_xlsx_download_indicator <-
    downloadHandler(
      filename = function()
      {
        paste0("INDICATOR-DATA",
               format(Sys.time(), "%Y-%m-%d-%h-%m-%s"),
               ".xlsx")
      },
      content = function(file)
      {
        indicator_values <-
          (hot_to_r(input$tbl_indicators)) %>% rownames_to_column()
        
        glimpse(indicator_values)
        
        wb <- createWorkbook()
        addCreator(wb, "3RP Data Collection - Agency Planning Tool")
        addWorksheet(wb, "INDICATORS")
        writeDataTable(
          wb,
          "INDICATORS",
          indicator_values,
          rowNames = TRUE,
          keepNA = FALSE,
          tableName =  "INDICATORS"
        )
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  
  observe({
    req(auth$result)  # <---- dependency on authentication result
    if (!is.null(input$tbl_indicators$changes$changes))
    {
      captured_values <-  unlist(input$tbl_indicators$changes$changes)
      old.val <- captured_values[4]
      new.val <- captured_values[3]
      if (!identical(old.val, new.val))
      {
        print("Update of tbl_indicators")
        indicators <-
          (hot_to_r(input$tbl_indicators)) %>% rownames_to_column()
        glimpse(indicators)
        if (values$planning_open)
          enable("to_db_indicators")
      }
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
