user_guide_tab <- function() {
  div(
    class = "container mw-100 px-5 overflow-auto",
    style = "height: calc(100vh - 100px)",
    div(class = "row",
      div(class = "col",
        h1(class = "mt-0", "Welcome"),
        p("This tool was developed by the Strategic Analysis, Research & Training (START) Center in the Department of Global Health at the University of Washington. The purpose of this tool is to summarize the costs and benefits of existing and new mosquito release programs across geographic settings and disease contexts."),
        h1("Acknowledgements"),
        p("Thank you to all of our collaborators:"),
        div(img(src = "acknowledgements.png", width = "100%"))
      ),
      div(class = "col",
        h1(class = "mt-0", "Instructions for Use"),
        tags$ol(
          tags$li(
            strong("Select your country of interest."),
            "Brazil, Colombia, Indonesia, Mexico, Sri Lanka, and Vietnam are included in the tool as geographies where mosquito release programs for dengue control would be implemented and scaled up. Burkina Faso is included in this tool as a demonstration of how this tool might be utilized for future technologies (e.g., gene drive) for malaria control. As such, many of the estimates and assumptions, particularly around cost, are uncertain for the malaria application."
          ), br(),
          tags$li(
            "Within each country,",
            strong ("enter the primary inputs, the estimated costs per kilometer squared for each program phase (planning, preparation, production, distribution, release, and monitoring)."),
            "Details on activities within each program phase are described in the \"Primary Input Parameters\" tab. A range of estimates and the median are included as defaults in the tool for each country."
          ), br(),
          tags$li(
            strong("Enter secondary inputs."), 
            "This includes the following: an estimated effectiveness of the intervention, ranging from 0-100% effective; area coverage, indicating what percentage of the relevant program area (0-100%) will be covered by the intervention; percent of cases treated in an ambulatory setting (0-100%); percentage of cases treated in a hospital setting (0-100%); mortality rate (0-100%); treatment cost per ambulatory visit; treatment cost per hospitalized visit; economic cost per death. Estimates from academic literature are included as defaults in the tool for each country."
          ), br(),
          tags$li(
            strong("Click Submit. "),
            "Based on these primary and secondary inputs, the tool will estimate several outputs and outcomes. "
          )
        ),
        h1("Contact Information"),
        tags$ul(
          tags$li("Aldina Mesic (amesic@uw.edu)"),
          tags$li("William Sheahan (wsheahan@uw.edu)"),
          tags$li("Jairam Lingappa (lingappa@uw.edu)"),
          tags$li("The START Center (start@uw.edu)")
        )
      )
    )
  )
}
