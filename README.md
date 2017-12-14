# moveVis GUI

## Introduction

moveVis GUI is a browser-based web application for animating movement data. It aims to provide an easy-to-use graphical user interface to all functionalities being present in the [moveVis R package](http://news.movevis.org). The application is still under development and will be embedded in a greater framework of movement analysis web applications that will be closely connected to [movebank.org](http://www.movebank.org). MoveVis GUI is based on the R package [moveVis](http://news.movevis.org) and was written in shiny.

## State of development

The published version is meant to be used only for demonstrational purposes. It works with exemplary data only, enabling its user to test the app's functionalities and behavior. A module connecting to movebank.org will be implemented within the scope of the framework development. 

## Installation

An operational use of this demo version of moveVis GUI is not recommended and not possible. To install the current version for playing around with the concept, use the `shiny` package:

```s
shiny::runGitHub("16eagle/moveVis_GUI")
```

The application will be downloaded to your local device and displayed in your default web browser.


## Appearance

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_GUI_preview1.png"></p>
<p align="center"><sub>Figure 1: Screenshot of the general settings tab (left) and the animation preview window (right)</sub></p>
<br>

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_GUI_preview2.png"></p>
<p align="center"><sub>Figure 2: Screenshot of the advanced settings tab (left) and the animation preview window (right)</sub></p>
<br>

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_GUI_processing.png"></p>
<p align="center"><sub>Figure 3: Screenshot of the advanced settings tab (left) and the animation processing window (right)</sub></p>
<br>

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/moveVis_GUI_download.png"></p>
<p align="center"><sub>Figure 4: Screenshot of the general settings tab (left) and the animation processing window, ready for download (right)</sub></p>
<br>


## Ideas

Ideas are welcome! Open an issue to start a discussion: <https://github.com/16eagle/moveVis_GUI/issues> 


## Acknowledgements

The moveVis web application is being development within a project framework aiming to build a online toolbox for analyzing movement data based on existing software libraries (such as R packages) at the Max Planck Institute for Ornithology. 

The underlying moveVis R package is being developed within the scope of the <a target="_blank" href="http://www.fernerkundung.geographie.uni-wuerzburg.de/forschung/projekte/laufende_projekte/opt4environment">Opt4Environment</a> project at University of Wuerzburg and was funded by the German Aerospace Center (DLR) on behalf of the Federal Ministry for Economic Affairs and Energy (BMWi) with the research grant <b>50 EE 1403</b>.

<p align="justify">
<a href="http://www.fernerkundung.geographie.uni-wuerzburg.de/en/lehrstuehle_und_arbeitsgruppen/department_of_remote_sensing/startseite//"><img width="150" height="100" src="https://www.uni-wuerzburg.de/typo3conf/ext/uw_sitepackage/Resources/Public/Images/uni-wuerzburg-logo.svg"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.dlr.de/eoc/en/"><img width="115" height="100" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/DLR_Logo.svg/744px-DLR_Logo.svg.png"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.bmub.bund.de/"><img width="220" height="100" src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRX92Q6lhYFo0Rv7p7Y3obqFXsxRyjXMNKSJ_q9bAvXYdFd5wOF3Q"></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="http://www.orn.mpg.de/en/"><img width="200" height="100" src="https://www.molgen.mpg.de/188611/mpi_Seew_LogoText-1355515314.gif"></a>
</p>