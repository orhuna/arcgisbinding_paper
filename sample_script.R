tool_exec <- function(in_params, out_params)
{
  library(hpiR, quietly = TRUE)
  library(raster, quietly = TRUE)
  library(sf, quietly = TRUE)
  ####### 1. Parse and Assign Input and Output Parameters from the Tool ########
  ## Read Input Parameters from the Tool 
  ### Input Feature Class Directory
  in_fc <- in_params[[1]]
  ### Input Feature Class Field for House Price
  price_field <- in_params[[2]]
  ### Input Feature Class Fields for Explanatory Variables
  x_vars <- in_params[[3]]
  ### Input Feature Class Field for Date
  date_field <- in_params[[4]]
  ### Input Feature Class Field for Unique House ID
  id_field <- in_params[[5]]
  ### Type of Regression Model
  model_type <- in_params[[6]]
  ### Distance Raster to be Used (if Exists)
  dist_raster <- in_params[[7]]
  
  ## Read Output Parameters from the Tool
  ### Define Output Feature Location
  out_fc <- out_params[[1]]
  
  ########## 2. Read In Data from the Tool into Spatial R Dataframes ###########
  ## Define All Fields to Read
  all.fields <- c("OBJECTID", x_vars, date_field, id_field, price_field)
  
  ## Display A Progress Label and Percent in the Tool UI at Runtime - Input
  ## Features
  pos <- 20
  arc.progress_label("Reading Input Feature Class")
  arc.progress_pos(pos)
  
  ## Read All Vector-Based Fields
  input.features <- arc.select(arc.open(in_fc), fields=all.fields)

  ## Convert the Input Features from arc Dataframe to sf Dataframe
  input.features.sf <- arc.data2sf(input.features)
  
  ## Display A Progress Label and Percent in the Tool UI at Runtime - Input 
  ## Raster
  
  ## If Given, Read the Input Distance Raster
  if (!is.null(dist_raster)){
    ### Update the Progress Bar in the Tool UI
    pos <- pos + 20
    arc.progress_label("Reading Input Distance Raster")
    arc.progress_pos(pos)
    
    ### Read Raster Input
    input.raster <- arc.raster(arc.open(dist_raster))
    
    ### Convert the arc type raster to R type (raster*) raster
    input.raster.R <- as.raster(input.raster)
    
    ### Update the Progress Bar in the Tool UI
    pos <- pos + 20
    arc.progress_label("Extracting Distance Raster Values at House Locations")
    arc.progress_pos(pos)
    
    ### Extract Values from the Distance Raster at House Locations
    #### Set the Coordinate System of the Raster to that of the Feature Class
    #### This is necessary for raster package's extract function to work
    crs(input.raster.R) <- CRS(st_crs(input.features.sf)$wkt)

    #### Extract Raster Values at Point (House) Locations
    distance.values <- extract(input.raster.R, 
                         method = 'bilinear', 
                         input.features.sf,
                         na.rm = FALSE)
    
    #### Get the Number of Houses without Distance Values
    n.dropped <- sum(is.na(distance.values))
    
    #### Append the Extracted Distance as a new Field 
    input.features.sf["distance"] <- distance.values
    pred.vars <- append(unlist(x_vars), "distance")
    
    if (n.dropped){
      print(paste0(n.dropped, "locations were dropped from the original input",
                   " feature class due to missing values at these locations",
                   " in the distance raster"))
      
    }else{
      print(paste0("Distance values for all house locations were extracted",
                   " successfully"))
    }
    
  }else{
    pos <- pos + 40
    print("No distance raster is provided.")
    
    pred.vars <- unlist(x_vars)
  }
  
  ## Display A Progress Label and Percent in the Tool UI at Runtime - Hedonic 
  ## Dataframe
  pos <- pos + 10
  arc.progress_label("Creating the House Transaction Dataframe")
  arc.progress_pos(pos)
  
  hedonic.df <- hedCreateTrans(input.features.sf,
                               id_field ,
                               "OBJECTID",
                               price_field,
                               date= date_field)
  
  ## Display A Progress Label and Percent in the Tool UI at Runtime - Hedonic 
  ## Dataframe
  pos <- pos + 10
  arc.progress_label(paste0("Creating House Trasaction Dataframe",
                     " Transaction Data"))
  arc.progress_pos(pos)
  
  ## Display A Progress Label and Percent in the Tool UI at Runtime - Hedonic 
  ## Dataframe
  pos <- pos + 10
  arc.progress_label(paste0("Fitting the Hedonic Regression Modely to the",
                            " Transaction Data"))
  arc.progress_pos(pos)
  
  #Parse the Inputs into Equation
  mode.eqn <- reformulate(pred.vars, paste0("log(", price_field,")"))
  hed.model <- hedModel(estimator = structure('robust', class = 'base'),
                        hed_df = hedonic.df, periodicity = 'monthly',
                        hed_spec = as.formula(mode.eqn))
  
  print(summary(hed.model))
  
  hedonic.df['Predicted'] <- exp(hed.model$fitted.values)
  
  ## Display A Progress Label and Percent in the Tool UI at Runtime - Write 
  ## Predictions
  pos <- 100
  arc.progress_label("Writing Output Feature Class")
  arc.progress_pos(pos)
  
  arc.write(out_fc, hedonic.df, overwrite = TRUE)
}