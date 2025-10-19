# Shiny Examples 

This folder contains a number of small examples highlighting common problems and solutions when developing Shiny apps. Most contain multiple versions of the same app, iteratively working towards a solution in multiple steps. Some examples use the same toy dataset, but they are not necessarily connected. All examples are thoroughly documented in the code.


## `adaptive-inputs`

This example uses billboard chart data and a simple app to illustrate creating input options from data, and guiding the user in 'drilling down' the dataset by filtering available data and showing inputs only when relevant. 

Topics covered include:
- creating UI elements fordata with a known structure but unknown values
- adaptively updating input options in the UI from server code
- adaptively hiding/showing UI elements

## `reactive-values` 

Uses the diamonds dataset to illustrate splitting application logic into reusable reactive values. 

Topics covered include:
- creating and using reactive values
- designing reactive application logic

## `debounce-and-buttons`

This example uses billboard data to show several techniques to take control of reactive flow through debouncing/throttling and triggering events through buttons. 

Topics covered include:
- throttling/debouncing reactive updates
- explicitly controlling reactive dependencies
- using event buttons

## `model-fitting` 

Uses the diamonds data set to illustrate a few basic apps for interactively fitting, visualizing, and making inferences from a simple linear model. 

Topics covered include:
- creating UI elements for data with a (partially) unknown structure
- using user inputs in model fitting and visualization (non-standard evaluation)

## `predictive-model`

Uses the diamonds data set to illustrate a basic app for making and explaining inferences from a simple linear model. This example replicates some of the funcitonality of the `model-fitting` example, but largely uses a different approach.

Topics covered include: 
- creating UI elements for data with a (partially) unknown structure
- adaptively hiding/showing UI elements
- explicitly controlling reactive dependencies
- creating a simple `explainer` object for a fitted model