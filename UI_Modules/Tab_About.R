##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Sat Nov 23 11:28:22 2024                 #####
#####       Purpose : Shiny UI for the second tab            #####
#####       Comments :                                       #####
##################################################################


Tab_UI_About <- function() {
  
  tabPanel("About",  icon = icon("circle-info"),
           HTML('<div style="margin-left: 20px; ">'),
           
           
           HTML(paste0(
           
           "<BR><H3>Summary</H3>
           <P>With recognition of the importance of soil moisture information for environmental monitoring and prediction, the National Collaborative Research Infrastructure Strategy (NCRIS) has engaged CSIRO via TERN Australia (Terrestrial Ecosystem Research Network) to undertake a national soil water monitoring and prediction initiative. 
           The project aims to:<UL>
            <li>establish a nationally distributed network of soil moisture monitoring sites, </li>
            <li>support the development of improved soil moisture prediction systems, </li>
            <li>provide openly accessible environmental data infrastructure, </li>
            <li>and develop long-term benchmark observations across Australian landscapes. </li>
            </UL></P>
            
           <P>The network is intended to complement and integrate with existing monitoring systems, including the national COSMOS soil moisture network and other regional sensor deployments.</P>
           <P>Approximately 100 soil moisture monitoring sites are planned for installation across Australia. The intention is that most of these will be established at existing TERN surveillance sites, with the remainder positioned across agricultural and environmentally significant landscapes. </P>
          <P> The project also contributes to ongoing development of the Soil Moisture Inference and Prediction System (SMIPS), a national soil moisture modelling framework being developed by CSIRO to support improved environmental prediction and monitoring.</P>
           
           <P>A more detailed explanation of the project is available <a href='Docs/National_Soil_Moisture_Monitoring_Project_Description.pdf'><b>HERE</b></a>.</P>
           
           <H3>Project Objectives</H3>
           
<P>The broader objectives of the project are to:<UL>
            <li>Develop a nationally coordinated and consistent soil moisture monitoring capability.
<li>Improve understanding of soil water dynamics across diverse Australian environments.
<li>Support calibration and validation of soil moisture prediction models.
<li>Provide long-term benchmark environmental observations.
<li>Support agricultural productivity and drought resilience applications.
<li>Improve integration between existing soil moisture monitoring efforts.
<li>Develop publicly accessible environmental datasets and infrastructure.
<li>Archive soils and associated metadata to support future scientific analysis.
<li>Support future applications of soil spectroscopy, pedotransfer modelling, and soil hydraulic characterisation.
</UL>
</P>

<P>An important aspect of the project is that the sensor observations are accompanied by
detailed soil information. Soil moisture measurements are strongly influenced by soil
properties including texture, bulk density, organic carbon, salinity, and soil structure.
Consequently, soil characterisation forms a key component of the installation and
calibration workflow.</P>

<H3>Acknowledgements</H3>

<H3>Contacts</H3>
           
           
           "
             
                     
            )),
                
           
          
           
           HTML('</div>') 
           
           
           
  )}
