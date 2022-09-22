
<h1 align="center">DAIAT - Agriculture Growth Cycle and Trade Dataset</h1>

<!-- HEADER -->

<br />
<p align="center">
  <a href="#">
    <img alt="Light" src="/Documents/ucc_logo_irish.svg" width="15%">
&nbsp; &nbsp; &nbsp; &nbsp;
  <img alt="Dark" src="/Documents/IRC LOGO_RGB.jpg" width="35%">
  </a>
  <br />
 <p align="center">
    <a href="https://www.ucc.ie/en/daiat/"><strong>Read more about the DAIAT initiative »</strong></a>
    <br />
  </p>
</p>

<!-- CONTENT -->

## About
The Data and Artificial Intelligence for African Trade (DAIAT) Initiative [https://www.ucc.ie/en/daiat/] is a consortium of European and African-based scholars and practitioners who focus their research, teaching and policy making to develop and promote data-driven decision-making tools for the expansion of inclusive and sustainable trade between the continents.

This repository contains the working files and all codes used to generate the "Agriculture Growth Cycle and Trade" database, which forms part of Work Program 1 of the DAIAT Initiative.

## Acknowledgements
The funding support of the Irish Research Council (IRC) through its New Horizon’s Grant Program is gratefully acknowledged.

## Work program 1: Identification of export opportunities
This work program is motivated by the need to expand African countries' exports at the extensive margin.

A decision-support model for export opportunity identification will be developed during this work program for Ethiopia, Kenya, Nigeria, Burkina Faso,  and Tanzania. These five countries have seen relatively fast growth in industrialization since 2010 and may need access to expanded markets to continue to raise productivity growth. 

For each of the five countries, the detailed opportunities to export to Ireland and for Ireland to export to these countries will be identified using advanced data analytics. Such mutual opportunity identification will support the engagement of Ireland with these countries based on an advantageous mutual expansion of trade. It will also generate a template for expanding to other EU countries.

The second aim of this work program would be to consider the availability of export data per region and firm - i.e., decentralized export data in Africa. Most governments (e.g., through the tax revenue departments) have access to this data but tend not to make this available. This may impose an information cost/gap on trade.

### How this repository works
This section first addresses the data used and its sources, and second, provides the method and approach used to consolidate, clean and construct the final database.

#### 1. Clone this repository

```bash
git clone git@github.com:Riaan-Rossouw/DAIAT.git
```

#### 2. Description of repositry files and folder

#### Folders
* 'Archive': Old files/scripts that are not used in current database construction
* 'CleanData': All input data, saved in cleaned format
* 'FinalDatabase': All results from scripts, intermittent and final database
* 'Literature': Articles used to identify key variables of interest
* 'RawData': all input data, saved in raw format
* 'SeasonalityPlots': Monthly agricultural commodity trade charts, to identify potential seasonality in trade data

#### Scripts
* '00_Collect_Data_vYYYY-MM-DD.r': downloads certain data sources via API's.
* '01_Clean_Data_vYYYY-MM-DD.r': collects data (from 'RawData'), cleans it (saved to 'CleanData'), then compiles the final database (saved to 'FinalDatabase').
* '02_Trade_Plots_vYYYY-MM-DD.r': loads monthly trade data (from 'CleanData'), plots trends over time within each country/area individually for each agricultural commodity (saved to 'SeasonalityFigures').

#### 3. Data used and sources
Refer to 'RawData' folder for detail. Also refer to the Final Project Report and Presentation for detail.

#### 4. Data consolidation, clean-up and database construction 
Refer to the Final Project Report and Presentation.

(Last updated on 2022-09-22)
