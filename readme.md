---
title: Shiny Workshop
author: Karel Kroeze, k.a.kroeze@utwente.nl
date: June 3rd, 2022
---

# Shiny Workshop Documentation

This repository contains the presentation slides and example code used during the BDSi Shiny Workshop on June 3rd, 2022. 

The `examples` folder contains three sets of examples for common problems/features. Presentation slides used during the workshop can be found in `slides`. `renv` contains metadata about the R environment used in this project, which together with `renv.lock` file can be used to exactly reproduce the results. Finally, `participantns` (will) contain(s) examples of apps created by workshop participants.

## Presentation slides
Presentation slides inn `.pptx` format are available in the [slides](./slides/) folder. Supporting images are also in that folder.

## Examples
A series of simplified examples explaining crucial concepts are available in the [examples](./examples) folder. Each example has its own subfolder, and multiple versions of the same app that highlight a problem/feature, and incrementally build to a solution.

### [Reactive values](./examples/reactive-values)
Reactivity - updating outputs based on changing inputs - is at the core of programming shiny Apps. But sometimes, a the process of creating outputs from inputs is more complex. You may need to fetch data from external sources, fit a model, or otherwise require intermediate values. 

This example shows how an app with two outputs based on the same linear model can use a shared reactive value containing the model as an intermediate value. 

### [Controlling events](./examples/debounce-and-buttons)
Building on the previous example, it is often not desirable to update outputs and/or intermediate values immediately for every change in inputs. 

Consider the common case of a search field. When the user types a query shiny triggers an input change for each character typed. If we update on each input change event, we may overload the R instance by repeatedly calculating intermediate values and creating outputs, or reach bandwith or API rate limits for external data sources.

This example shows two approaches to control when reactive values are allowed to update, debouncing (and closely related, throttling), and triggers. 

### [Adaptive inputs](./examples/adaptive-inputs)
An extremely common input is to allow the user to select a variable from a dataset, for example to choose which outcome(s) and/or predictor(s) to show in a plot. You may then also need to update the UI to reflect the choices made.

The example uses music chart data to show how to populate inputs choices from the data, generating options based on artists listed in the data. It then expands on this topic by implementing a conditional input to select a track that only appears when the user has selected an artist, and dynamically updates the available track choices based on the chosen artist. 

