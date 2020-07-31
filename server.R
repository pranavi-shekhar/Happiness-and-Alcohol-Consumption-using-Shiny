library(shiny)
library(dplyr)
library(plotly)
library(data.table)
library(htmlwidgets)


shinyServer(function(input, output,session) {
    
    ## Read the data and preprocess -> convert values given as "noth" to "north"
    data = read.csv("alcohol.csv",header = TRUE)
    data$Hemisphere[grep("noth",data$Hemisphere)] = "north"
    
    
    ## Collect data from hovering on plotly plot
    datahover = reactive({
        event.data = event_data("plotly_hover")
        
    })

    
    ## Collect data from selecting region on plotly plot
    dataselected = reactive({
        event.data = event_data("plotly_selected")
        
    })
    
    
      ## Get regression model (slope and intercept) for the points in the entire region/hemisphere to which         the point the user hovers on belongs to - points in the plot are grouped by region/hemisphere. For         instance, if the user hovers on a point that is grouped into the hemisphere "north", then the model         will contain the slope and intercept for the entire set of points on the plot coming under "north". 
      ## ONLY HOVER EVENTS ARE VALID FOR GROUPED DATA
    model1 = reactive({
         
            if(!is.null(datahover()))
            {
                # X value of all data points belonging to that region/hemisphere (being hovered over)
                xc = datahover()[[1]]
                
                # Name of the region/hemisphere being hovered over, index in event.data is 5
                category = datahover()[[5]] 
                
                #Select only necessary columns(optional) and filter out values belonging to that region/hemisphere (being hovered over)
                
                temp = select(data,4,as.numeric(input$xaxis),as.numeric(input$colorby))
                temp = filter(temp,temp[,3]==category)
                
                # Create data frame containing only x and y values from temp, for easy access(optional)
                df = data.frame(x = temp[,2] , y = temp[,1])
                
                # Final model
                lm(y ~ x  , data=df )
            }
            
            else{NULL}
         
    })
    
    ## Get regression model for selected points - not grouped by any criteria
    
    ## ONLY SELECTION EVENTS ARE VALID FOR UN-GROUPED DATA
    model2 = reactive({
        
        if(!is.null(dataselected()))
        {
            # When we switch the X axis metric, the regression line from the previous selection persists,
            # since previous dataselected() exists. To avoid this, we specify another metric - the key. The               key for dataselected() is specified as the X axis metric itself (in the plotly plot,see below               ). So, the selected data (stored or otherwise) is plotted on screen only if the plot being                 displayed has a same X axis as dataselected(). This is intuitive and can be verified by                    deleting the below 'if' condition. Unlist is used as list formats cannot be accessed directly
          
            if(input$xaxis == unlist(dataselected()$key)[[1]])
            {
                lm(y~x,data = dataselected())
            }
            else{NULL}
            
        }
        else{NULL}
    })
    
    
    ## This section is to dynamically change the region/hemisphere labels in slope and intercepts,as the          user hovers over a point.
    category = reactive({
        if(input$colorby == 2)
        { 
            s1 = "Region" 
            s2 = unlist(datahover()[[5]])
            paste("<b>",s1," : ","</b>",s2)
        }
        else if(input$colorby == 3)
        { 
            s1 = "Hemisphere" 
            s2 = unlist(datahover()[[5]])
            paste("<b>",s1," : ","</b>",s2)
        }
        else if(input$colorby == 0)
        {
            ""
        }
        
        
    })
    
    ## Display current region/hemisphere user is hovering over
    output$category = renderText({category()})
    output$category2 = renderText({category()})
    
    ## Get slope value from relevant model -  grouped is model1 and un-grouped is model2
    output$slope = renderText({
        
        #This consdition checks if there is any grouping
        if(input$colorby == 2 || input$colorby == 3){
            
            if(is.null(model1()))
            {
                "No Model Found"
            }
            else
            {
                model1()[[1]][2]
            }
        } 
        else if(input$colorby == 0)
        {
            if(is.null(model2()))
            {
                "No Model Found"
            }
            else
            {
                model2()[[1]][2]
            }
        }

    })

    ## Get intercept value from relevant model -  grouped is model1 and un-grouped is model2
    output$intercept = renderText({
        if(input$colorby == 2 || input$colorby == 3){
            
            if(is.null(model1()))
            {
                "No Model Found"
            }
            else
            {
                model1()[[1]][1]
            }
        } 
        else if(input$colorby == 0)
        {
            if(is.null(model2()))
            {
                "No Model Found"
            }
            else
            {
                model2()[[1]][1]
            }
        }
         
    })

    
    ## Constructing the output plots using plotly
    
    output$plot1 = renderPlotly({
      
        # Get selected values for X axis and grouping - colorby tells us the metric to be group by. In the radio button selection, the values of selected items are the column numbers in the data set. For instance, the value obtained when teh user selects "Beer" is 7, which is the column number of Beer_PerCapita in the data set. 
        num = as.numeric(input$xaxis)
        col = as.numeric(input$colorby)
        
        # Keep only relevant columns from data - HappinessScore is column 4. 
        d = select(data,4,num,col,1)
        
        #This is the name of the X axis variable, useful for plotly tooltips.
        xvariable = colnames(data)[num]
        
        # If col is not 0, it means it is either 2 or 3 - plot with different colors for each region/hemisphere.
        #d[,2] contains the X Variable selected. d[,3] is basically the region/hemisphere to group by
        # Since checkbox for showing/hiding legend returns true/false the same can be supplied in layout
        # A linear regression line is added for each group in the plot. This leads to as many lines as the number of regions/hemispheres.Again, their visibility is also toggled by a legend, if necessary.
        # Plotly event "hover" is not available by default and needs to be registered
        
        if(col!=0)
        {
            plot =  plot_ly() %>% 
            add_trace(data = d, x = ~d[,2], y = ~HappinessScore, color = ~d[,3], colors = "Set1",mode="markers",type="scatter",key=~d[,3], hovertemplate = paste(xvariable,' : %{x}<br>','Happiness Score : %{y}<br>'), showlegend=input$legend, marker = list(size=7)) %>% 
              
            layout(xaxis = list(title = xvariable,zeroline = FALSE) ,  yaxis = list(title = "Happiness Score"),legend = list(orientation = "h",y = -0.6,font = list(size = 7))) %>% 
              
            add_trace(key=~d[,3],x = ~d[,2] , y = fitted(lm(d[,1] ~ d[,2]*d[,3])),mode = "lines", color = ~d[,3],type = "scatter", showlegend=input$legend, visible = input$reglines,hovertemplate = paste(xvariable,' : %{x}<br>','Happiness Score : %{y}<br>')) %>%  
              
            event_register("plotly_hover")  
      
  
              
        }
 
        else
        {
            # Here, the regression lines are not plotted by default - only if a certain region of data points are selected. The lines are plotted for the selected set of points ONLY.
          
            plot  = plot_ly()%>% 
              
            add_trace(data = d, x = ~d[,2], y = ~HappinessScore ,mode = "markers",key=~input$xaxis,hovertemplate = paste(xvariable,' : %{x}<br>','Happiness Score : %{y}<br><extra></extra>'),showlegend=F, marker = list(color = "#3366ff", size=7)) %>% 
              
            layout(xaxis = list(title = colnames(data)[num],zeroline = FALSE ) ,  yaxis = list(title = "Happiness Score"))      
            
            # Do this only if model2 is available - i.e. specific data points are selected
            # shapes is used to highlight the selected points by creating a box around them
            if(!is.null(model2()))
            {
               
                plot%>% add_trace(x=~dataselected()$x, y=fitted(model2()), mode="lines",showlegend=F, line = list(color = "#66cc00")) %>% layout(shapes = list(type="rect", x0=min(dataselected()$x)-10, y0=min(dataselected()$y), x1=max(dataselected()$x)+10, y1=max(dataselected()$y), opacity=0.4))
            }
            else{plot}

        }
        
        
    })
    
    
    ## To display the data set, in a different tab(controlled by UI)
    output$showdata = renderTable({
        
        # Obtain regions and hemispheres present in data
        regions = levels(factor(data$Region))
        hemispheres = levels(factor(data$Hemisphere))
        
        # Obtain range of happiness score
        min = input$hscore[1]
        max = input$hscore[2]
        
        # tolower() is used because in the drop down list, for hemispheres the first letter is capitalized, for display purposes, whereas in the data set it's all in lower case
        if(tolower(input$filter) %in% hemispheres)
        {
          #input$numrows deals with range of rows (by row numbers) you wish to see. For instance, setting the slider to 2-10 will display rows 2-10. 
          # min and max represent the range of happiness scores the user wishes to view 
          filter(data[input$numrows[1] : input$numrows[2],], Hemisphere == tolower(input$filter) & (min<=HappinessScore & HappinessScore<=max))
        }
        else if(input$filter %in% regions)
        {
          filter(data[input$numrows[1] : input$numrows[2],], Region == input$filter& (min<=HappinessScore & HappinessScore<=max))
        }
        else if(input$filter=="None")
        {
          filter(data[input$numrows[1] : input$numrows[2],], min<=HappinessScore & HappinessScore<=max)
        }
        
    },include.rownames=TRUE)
    
    # Summary or description of the dataset
    output$datadesc = renderText({
        desc = HTML('<ul>
	<li>This data set aims to explore the happiness levels of individuals based off of their per capita consumption of alcoholic beverages, Human Development Index(HDI) and GDP</li>
	<li>Happiness Score is measured on a scale of 1-10 with 10 being the happiest. The data set shows the average happiness score indicated by the country\'s residents</li>
	<li>The HDI used is provided by the United Nations. Further, the region and hemisphere of each country has been specified</li>
	<li>The unit for per capita consumption of beverages is Liters. Again, the country wise average is shown</li>
	<li>More details regarding the data set can be found here : www.kaggle.com/marcospessotto/<br>happiness-and-alcohol-consumption</li>
</ul>') 
    })
})