
tabPanel("Select an Individual",
         wellPanel(HTML(
           paste(
             "<h1>Proof of Concept</h1>",
             "<p>This is a proof of concept for selecting an individual and seeing their connections.</p>",
             "<p>The following features are provided:</p>",
             "<ul>",
             "<li>Select a name from the drop down menu (note additional fields may be added)</li>",
             "<li>The individuals person details are displayed beneath</li>",
             "<li>The network below includes only those individuals connected to the selected individual</li>",
             "<li>Hovering over nodes displays the individual's name</li>",
             "</ul>",
             "<p>Please note no effort has been made to catch errors and while loading error messages
             may show - this can be fixed with minimal effort.</p>",
             "<p><em>Note:</em> It would be possible to modify this screen to allow n-degree neighbours of the selected
             individual to be displayed, this would require ~1 hour development time</p>",
             sep = ""
           )
         )),
         
         uiOutput("select.individual_UI"),
         h2("Personal Details of selected individual"),
         tableOutput("select.individual_personInfo"),
         h2("Direct Connections of selected individual"),
         visNetworkOutput("select.individual_visNetwork")
                     
         
         
#          
#          
#          fluidRow(
#            column(width = 4, 
#                   checkboxInput("hierachical_layout_option", label = "Hierarchical Layout?",
#                                 value = TRUE),
#                   uiOutput("event_category_selection_UI"),
#                   uiOutput("event_type_selection_UI")
#            ),
#            column(width = 8,
#                   visNetworkOutput("visNetwork_example",width = "100%", height = "600px"))
#          ),
#          conditionalPanel("typeof input.current_node_id !== 'undefined'",
#                           wellPanel(HTML(
#                             paste0(
#                               "<h2>",textOutput("selected.individual.name", inline = TRUE),"'s Connections</h2>",
#                               "<p>The table below shows all life events involving the selected individual, 
#                               note the controller allows columns to be added and removed easily.</p>"
#                             ))),
#                           uiOutput("connected_life_events_columns_to_show_UI"),
#                           DT::dataTableOutput("selected_node")
#                             )
         
         
)