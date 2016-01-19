
tabPanel("networkD3 - Whole Network",
         wellPanel(HTML(
           paste(
             "<h1>Proof of Concept</h1>",
             "<p>The graph below is a proof of concept for visualising EMLO data.</p>",
             "<p>The following features are provided:</p>",
             "<ul>",
             "<li>Tooltips on vertices (showing name of individual, this can be customised)</li>",
             "<li>The controls on the left allow the user to select the categories/event types they are interested in</li>",
             "<li>Click a node highlights connected individuals, scroll down to see information</li>",
             "</ul>",
             "<p>Please note no effort has been made to catch errors and while loading error messages
             may show - this can be fixed with minimal effort.</p>",
             "<p><em>Note:</em> Disabling 'hierarchical layout' is likely to create a mess at present</p>",
             sep = ""
           )
         )),
         
         fluidRow(
           column(width = 4, 
                  uiOutput("networkD3_wholeNetwork_HighlightedCategoryUI"),
                  h2("Categories to Exclude from Visualisation"),
                  uiOutput("networkD3_wholeNetwork_ExcludedCategoriesUI"),
                  uiOutput("networkD3_wholeNetwork_NumberOfExcluded")
           ),
           column(width = 8,
                  forceNetworkOutput("networkD3_wholeNetwork",width = "100%", height = "600px"))
         ),
         conditionalPanel("typeof input.current_node_id !== 'undefined'",
                          wellPanel(HTML(
                            paste0(
                              "<h2>",textOutput("network3D_wholeNetwork_selected_individual_name", inline = TRUE),"'s Connections</h2>",
                              "<p>The table below shows all life events involving the selected individual, 
                              note the controller allows columns to be added and removed easily.</p>"
                            ))),
                          uiOutput("connected_life_events_columns_to_show_UI"),
                          DT::dataTableOutput("selected_node")
                            )
         
         
)