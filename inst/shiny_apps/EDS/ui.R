
library(shiny)

shinyUI(
  fluidPage(

    includeCSS("www/custom.css"),

    fluidRow(

      column(1,style="height:65px",
             h1("EDS")
      ),
      column(4,style="height:65px",
             h4("Evaluation by Dynamical Simulation (v0.9)",style="padding:21px;")
      ),

      column(4,offset=3,style="padding:23px;height:65px",
             fluidRow(

               column(10,
                      img(src = "MSC.png", height = 37, width = 60),
                      img(src = "DLMtool.png", height = 40, width = 130),
                      img(src = "NRDC.png", height = 40, width = 30),
                      img(src = "UBC.png", height = 40, width = 30))


             )
      ),

      column(12,
             fluidRow(

               h5(" "),
               HTML("<br>"),

               column(width = 4, style="height:360px",
                      tabsetPanel( id = "tabs1",
                        tabPanel(h4("Fishery",style = "color:black"),

                            conditionalPanel(width=4,condition="output.Fpanel==undefined|output.Fpanel==0",

                                 HTML("<br>"),
                                 h5("The Fishery panel is a set of questions about the characteristics of the fish population and its fishery.",style="color:grey"),
                                 h5("These questions specify the range of simulations in the MSE.",style="color:grey"),
                                 h5("Questions are presented in order of importance.",style="color:grey"),
                                 h5("At any stage you can press the CALCULATE button and the MSE will run for the questions you have answered",style="color:grey"),
                                 h5(""),
                                 tagList("More detailed help on the Fishery questions can be found in the
                                         DLMtool-MSC User Guide: ", a("Section 4", href="https://dlmtool.github.io/DLMtool/userguide/index.html"))),

                            conditionalPanel(width=4,condition="output.Fpanel==1",#|output.Fpanel==undefined",
                                  h5("1. Fishery description"),
                                  column(width=12,style="height:40px",
                                         textInput("Name", "", "Name of fishery (e.g. 'Atlantic swordfish')")),
                                  column(width=12,style="height:40px",
                                         textInput("Species","","Species (e.g. 'Xiphias gladius')")),
                                  column(width=12,style="height:40px",
                                         textInput("Region","","Region (e.g. 'North Atlantic')")),
                                  column(width=12,style="height:40px",
                                         textInput("Agency","","Management Agency (e.g. 'ICCAT')")),
                                  column(width=12,style="height:40px",
                                         textInput("nyears", "", "Years of historical fishing (e.g. '68' years since 1951)")),
                                  column(width=12,style="height:40px",
                                         textInput("Author", "","Author of this analysis ('Alex Smith (a.smith@gmail.com)')"))
                             ),

                            conditionalPanel(width=4,condition="output.Fpanel==2",#|output.Fpanel==undefined",
                                checkboxGroupInput("M", label = h5("2. Longevity",style="color:black"),
                                    choices = M_list, selected = M_list),
                                actionLink("All_M","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==3",
                                checkboxGroupInput("D", label = h5("3. Stock depletion",style="color:black"),
                                     choices = D_list, selected = D_list),
                                actionLink("All_D","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==4",
                                checkboxGroupInput("h", label = h5("4. Resilience",style="color:black"),
                                        choices = h_list, selected = h_list),
                                actionLink("All_h","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==5",
                                checkboxGroupInput("FP", label = h5("5. Trend of historical exploitation",style="color:black"),
                                        choices = FP_list, selected = FP_list),
                                actionLink("All_FP","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==6",
                                 checkboxGroupInput("F", label = h5("6. Variability in historical exploitation",style="color:black"),
                                        choices = F_list, selected = F_list),
                                 actionLink("All_F","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==7",
                                 checkboxGroupInput("sel", label = h5("7. Selectivity relative to length at maturity",style="color:black"),
                                        choices = sel_list, selected = sel_list),
                                 actionLink("All_sel","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==8",
                                 checkboxGroupInput("dome", label = h5("8. Selectivity of large fish",style="color:black"),
                                        choices = dome_list, selected = dome_list),
                                 actionLink("All_dome","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==9",
                                 checkboxGroupInput("DR", label = h5("9. Discard rate",style="color:black"),
                                        choices = DR_list, selected = DR_list),
                                 actionLink("All_DR","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==10",
                                 checkboxGroupInput("PRM", label = h5("10. Post-release mortality rate",style="color:black"),
                                         choices = PRM_list, selected = PRM_list),
                                 actionLink("All_PRM","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==11",
                                 checkboxGroupInput("sigR", label = h5("11. Recruitment variability",style="color:black"),
                                         choices = sigR_list, selected = sigR_list),
                                 actionLink("All_sigR","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==12",
                                  checkboxGroupInput("q", label = h5("12. Changing fishing efficiency",style="color:black"),
                                         choices = q_list, selected = q_list),
                                  actionLink("All_q","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==13",
                                  checkboxGroupInput("A", label = h5("13. Size of potential / existing marine reserve",style="color:black"),
                                         choices = A_list, selected = A_list),
                                  actionLink("All_A","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==14",
                                  checkboxGroupInput("V", label = h5("14. Spatial mixing (movement)",style="color:black"),
                                         choices = V_list, selected = V_list),
                                  actionLink("All_V","UNKNOWN")),

                            value=1),


                        tabPanel(h4("Management",style = "color:black"),

                                 conditionalPanel(width=4,condition="output.Mpanel==undefined|output.Mpanel==0",

                                      HTML("<br>"),
                                      h5("The Management panel is a set of questions about what fishery management options are available and how well management advice is followed.",style="color:grey"),
                                      h5("These questions: ",style="color:grey"),
                                      h5(" - identify what management procedures are feasible given the types of management measures.",style="color:grey"),
                                      h5(" - determine the relative success of various management procedures that provide different types of advice",style="color:grey"),
                                      h5(""),
                                      tagList("More detailed help on the Management questions can be found in the DLMtool-MSC manual:
                                              ", a("Section 5", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081"))),


                                 conditionalPanel(width=4,condition="output.Mpanel==1",
                                                  checkboxGroupInput("M1", label = h5("1. Types of fishery management that are possible",style="color:black"),
                                                                     choices = M1_list, selected = M1_list),
                                                  actionLink("All_M1","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Mpanel==2",
                                                  checkboxGroupInput("IB", label = h5("2. Implementation uncertainty: consistent overages/underages",style="color:black"),
                                                                     choices = IB_list, selected = IB_list),
                                                  actionLink("All_IB","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Mpanel==3",
                                                  checkboxGroupInput("IV", label = h5("3. Implementation uncertainty: variability",style="color:black"),
                                                                     choices = IV_list, selected = IV_list),
                                                  actionLink("All_IV","UNKNOWN")),

                                 value=2),

                        tabPanel(h4("Data",style = "color:black"),

                                 conditionalPanel(width=4,condition="output.Dpanel==undefined|output.Dpanel==0",

                                  HTML("<br>"),
                                  h5("The Data panel is a set of questions about what types of data are available and quality of the data that are available",style="color:grey"),
                                  h5("These questions: ",style="color:grey"),
                                  h5(" - identify what management procedures are feasible given the types of data available.",style="color:grey"),
                                  h5(" - determine the relative success of the various management types that rely on differing types of data",style="color:grey"),
                                  h5(""),
                                 tagList("More detailed help on the Data questions can be found in the DLMtool-MSC manual
                                         : ", a("Section 6", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081"))),

                                 conditionalPanel(width=4,condition="output.Dpanel==1",
                                                  checkboxGroupInput("D1", label = h5("1. Types of data that are available",style="color:black"),
                                                                     choices = D1_list, selected = D1_list),
                                                  actionLink("All_D1","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Dpanel==2",
                                                  checkboxGroupInput("CB", label = h5("2. Catch reporting bias",style="color:black"),
                                                                     choices = CB_list, selected = CB_list),
                                                  actionLink("All_CB","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Dpanel==3",
                                                  checkboxGroupInput("Beta", label = h5("3. Hyperstability in indices",style="color:black"),
                                                                     choices = Beta_list, selected = Beta_list),
                                                  actionLink("All_Beta","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Dpanel==4",
                                                  radioButtons("Err", label = h5("4. Overall data quality",style="color:black"),
                                                                     choices = Err_list, selected = "Err_bad"),
                                                  actionLink("All_Err","DEFAULT")),
                                 value=3),

                        tabPanel(h4("Help",style = "color:black"),
                                # textOutput(
                                 h5("1. Specify fishery, management and data attributes",style = "color:grey"),
                                 h5("2. Save progress",style = "color:grey"),
                                 h5("3. Run MSE by selecting CALCULATE (5 minutes)",style = "color:grey"),
                                 h5("4. Visualize results",style = "color:grey"),
                                 h5("5. Build report",style = "color:grey"),
                                 h5("6. Determine ancillary indicators",style = "color:grey"),
                                 h5("7. Calculate value-of-information",style = "color:grey"),
                                 HTML("<br>"),
                                 tagList("For further information see the ", a("MSC-DLMtool Manual", href="https://www.datalimitedtoolkit.org/")),
                                 HTML("<br>"),
                                 tagList("The DLMtool paper is also available ", a("here", href="https://www.datalimitedtoolkit.org/")),
                                 value=4)
                      )

               ),


               column(width = 8,style="height:360px",

                      HTML("<br><br>"),
                      hr(),

                      # --------- Fishery panel guides ---------------------------------------------------------------------------------------------------------------

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==0",
                          h5("",style = "color:grey")
                      ),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==1",

                          column(12,
                            h5("Describe the fishery you are modelling and identify yourself and the relevant management agency",style = "color:grey")

                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==2",
                          column(6,plotOutput("plotM",height=240)),

                          column(6,
                            h5("How long lived is the fish species? This is a critical input determining stock productivity",style = "color:grey"),
                            h5("The parameter M is the instantaneous natural mortality rate. For a review of data-limited methods of estimating M see",style = "color:grey"),
                            tagList(a("Kenchington (2014)", href="http://onlinelibrary.wiley.com/doi/10.1111/faf.12027/abstract")),

                            h5("The plot to the left shows survival rates at age for the longevity scenarios you have selected",style = "color:grey"),
                            h5("The range in the maximum age is plotted in orange and reflects the age at 2% survival for the most extreme longevity scenarios that were selected",style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==3",
                          column(6,plotOutput("plotD",height=240)),
                          column(6,
                            h5("Depletion D, refers to current spawning stock biomass relative to unfished.",style = "color:grey"),
                            h5("Since depletion is a data-rich quantity it may not be readily quantified and it may be necessary to specify a wide range of uncertainty for this input to identify MPs that are suitably robust.",style = "color:grey"),
                            h5("In a data-limited situation, coarse information regarding depletion may be obtained from examining length compositions, historical versus current catch rates, or by use of so-called Robin-Hood approaches.",style = "color:grey"),
                            tagList("For further information see ", a("Carruthers et al. (2014)", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081")),
                            tagList("and ", a("Punt et al (2011)", href="https://academic.oup.com/icesjms/article/68/5/972/653125/Among-stock-comparisons-for-improving-stock"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==4",
                          column(6,plotOutput("ploth",height=240)),
                          column(6,
                            h5("How resilient to exploitation is the stock? This question controls recruitment compensation - the extent to which recruitment is reduced from unfished levels (R0) as the spawning stock becomes increasingly depleted below unfishe levels (SSB0).
                             Here resilence is expressed in terms of steepness (h): the fraction of unfished recruitment at 1/5 unfished spawning biomass",style = "color:grey"),
                            tagList("For a useful review of compensatory density dependence in fish populations see ", a("Rose et al. (2001)", href="http://hqczhzkgch48vzcc4czubj6v.wpengine.netdna-cdn.com/files/2012/07/Rose_etal_FishFisheries.pdf"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==5",
                          column(6,plotOutput("plotFP",height=240)),
                          column(6,
                            h5("What temporal pattern best describes the trend in historical exploitation rates (aka fishing mortality rates or F)?. If more than one answer is given, historical fishing will be simulated subject to all trends in equal frequency. If it can be assumed that fishing effort is proportional to exploitation rate this can be summarized as historical patterns in fishing effort (e.g. boat-days per year, number of trips per year).
                             This set of answers specified the possible range of mean trends, you will have an opportunity to adjust the extent of interannual variability in the following question.",style = "color:grey"),
                            tagList("Here is a basic introduction to fishing effort and the assumption of proportionality courtesy of the ", a("UN FAO", href="http://www.fao.org/docrep/x5685e/x5685e04.htm"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==6",
                          column(6,plotOutput("plotF",height=240)),
                          column(6,
                            h5("The extent of interannual variability in historical exploitation rates around the mean trend(s) specified in question F4.",style = "color:grey"),
                            tagList("Again, here is the introduction to effort and exploitation rate by the ", a("UN FAO", href="http://www.fao.org/docrep/x5685e/x5685e04.htm"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==7",
                          column(6,plotOutput("plotsel",height=240)),
                          column(6,
                            h5("Fishing gear selectivity relative to size at maturity (S). For example, if 50% of 40cm fish are caught by the gear and length at 50% maturity is 50cm, S = 4/5.",style = "color:grey"),
                            tagList("The UN FAO explain something of fishing gear selectivity and how it may be quantified in an ", a("introductory document", href="http://www.fao.org/docrep/w5449e/w5449e08.htm")),
                            tagList("For a more involved discussion on selectivity see the ", a("IATTC CAPAM workshop report", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==8",
                         column(6,plotOutput("plotdome",height=240)),
                         column(6,
                           h5("Fishing gear selectivity of the largest individuals (SL). For example, if only 20% of the longest fish are caught by the gear SL = 0.2.",style = "color:grey"),
                           tagList("Again here is the UN FAO introduction to fishing gear selectivity ", a("introductory document", href="http://www.fao.org/docrep/w5449e/w5449e08.htm")),
                           tagList("and here is the ", a("IATTC CAPAM workshop report", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==9",
                          column(6,plotOutput("plotDR",height=240)),
                          column(6,
                            h5("Discard rate (DR). What fraction of fish that are caught are discarded (includes fish that are dead and alive)?",style = "color:grey"),
                            tagList("The US National Marine Fisheries Service have a general guide to ", a("Understanding Fish Bycatch Discard and Escapee Mortality", href="https://www.afsc.noaa.gov/quarterly/jfm03/featurejfm03.pdf")),
                            tagList("and one of the authors of that guide, Michael Davis also has a useful article: ", a("Key principles for understanding fish bycatch discard mortality", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==10",
                          column(6,plotOutput("plotPRM",height=240)),
                          column(6,
                            h5("Post-release mortality rate (PRM). What fraction of discarded fish die after release?",style = "color:grey"),
                            tagList("The US National Marine Fisheries Service have a general guide to ", a("Understanding Fish Bycatch Discard and Escapee Mortality", href="https://www.afsc.noaa.gov/quarterly/jfm03/featurejfm03.pdf")),
                            tagList("and one of the authors of that guide, Michael Davis also has a useful article: ", a("Key principles for understanding fish bycatch discard mortality", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==11",
                           column(6,plotOutput("plotsigR",height=240)),
                           column(6,
                             h5("Interannual variability in recruitment (the coefficient of variation in log-normal recruitment deviations, sigma R). Recruitment is expected to change among years in response to changing spawning biomass levels. On top of this is additional variability that may be driven by many factors including varying ocean conditions, amount of spawning habitat, food availability and predation.  Sigma R controls the extent of this additional variability in annual recruitments. For example, a sigma R of 10% means that 95% of recruitments will fall in approximately 80-120% of the mean recruitment predicted from spawning biomass",style = "color:grey"),
                             tagList("Edward Houde authored a ", a("Comprehensive Review of Recruitment and Sources of Variability", href="https://drive.google.com/open?id=19q_ct4Xd954H2jjotX3YKy0BJ-v53bt2")),
                             tagList("and if that isn't sufficient there is ", a("Chambers and Trippel (1997)", href="https://drive.google.com/open?id=1KhwP5URGPzW6ViJPiwprfY2tE2uucpDR"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==12",
                           column(6,plotOutput("plotq",height=240)),
                           column(6,
                             h5("The annual % increase/decrease in fishing efficiency (q). In targetted fisheries gear efficiency may improve over time given techological improvements in the gear, bait, fish location, sharing of information etc. Conversely, non-target or bycatch species may be subject to declining fishing efficiency due to regulations or avoidance behaviors. The catchability q is the fraction of available fish caught per unit of effort. For example, a 2% per annum increase in fishing efficiency means that after 35 years twice as many fish will be caught for the same effort as today.",style = "color:grey"),
                             tagList("The introduction to fishing efficiency by the FAO provides a ", a("basic summary", href="http://www.fao.org/docrep/008/y6982e/y6982e06.htm")),
                             tagList("alternatively there is a more comprehensive review by", a("Arrenguin-Sanchez", href="https://drive.google.com/open?id=1ZrHbhotCJ5Vjw4JNloUSY94BVoM0iyfI"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==13",
                           column(6,plotOutput("plotA",height=240)),
                           column(6,
                             h5("The size of a potential or existing spatial closure (Marine Reserve). The size S is the % of habitat that is protected.",style = "color:grey"),
                             tagList("The FAO provides a comprehensive ", a("review of Marine Protected Areas", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf"))
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==14",
                           column(6,plotOutput("plotV",height=240)),
                           column(6,
                            h5("The degree of stock mixing. The probability P of a fish leaving area 1 (ie. the MPA) between years",style = "color:grey"),
                            tagList("The FAO provides a comprehensive ", a("review of Marine Protected Areas", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf"))
                      )),


                      # -------- Management panel guides --------------------------------------------------------------------------------------------------------------

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==0",
                          h5("",style = "color:grey")
                      ),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==1",
                         # h5("Possible modes of fishery management",style = "color:black"),
                          h5("Here you indicate which MPs are feasible given the management options that are available.", style = "color:grey"),
                          h5("Management procedures can provide management advice in terms of:",style="color:grey"),
                          h5(" - Total Allowable Catch limits (TACs, e.g. 20,000 metric tonnes)",style="color:grey"),
                          h5(" - Total Allowable Effort (TAE, e.g. 800 trap days per year)",style="color:grey"),
                          h5(" - Size limits (e.g. minimum size of 45cm)",style="color:grey"),
                          h5(" - Time-area closures (e.g. closing an area to fishing, an MPA or a Winter closure",style="color:grey"),
                          h5(""),
                          tagList("For more information see the ", a("UN FAO guide to fishery management types", href="http://www.fao.org/docrep/005/y3427e/y3427e06.htm")),
                          h5(""),
                          tagList("Or alternatively, Steffanson and Rosenberg describe and discuss fishery managment types in ", a("their 2005 paper", href="https://drive.google.com/open?id=1V5aMNf3raitNC515qyFfITDivgbXkU4X"))

                      ),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==2",
                          column(7,plotOutput("plotIB",height=280)),
                          column(5,
                                 h5("What is the possible extent to which fishing operations may exceed (overages) or fall short (underages)
                                     with respect to the specified Total Allowable Catch (TAC)and Effort (TAE). For example, given a TAC of 1000 tonnes and a TAE of 2000 boat-days of fishing a 10% overage would on average lead to 1100 tonnes of fish taken and 2200 boat days of effort.",style = "color:grey"),
                                 h5(""),
                                 tagList("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm")),
                                 h5(""),
                                 tagList("Fulton et al. provide a discussion of implementation error in their ",a("2011 paper", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h"))
                      )),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==3",
                          column(7,plotOutput("plotIV",height=280)),
                          column(5,
                                h5("In question M2 you specified the range of possible overages/underages with respect to management recommendations. In this question you now also add the variability (V) in the implementation (TACs and TAEs) among years. For example if on average there are no overages or underages, V = 10% leads to overages/underages within 20% of the recommendation (the black line in the figure opposite) for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest (solid line) levels of overages/underages specified in question M2 ",style = "color:grey"),
                                h5(""),
                                tagList("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm"))
                      )),


                      # -------- Data panel guides --------------------------------------------------------------------------------------------------------------------

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==0",
                          h5("",style = "color:grey")
                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==1",
                          h5("Management procedures require various data. Where data types are unavailable some MPs
                              may not be feasible.", style = "color:grey"),
                          h5("Annual catches are yearly reporting landings (e.g. 135 tonnes in 1998, 159 tonnes in 1999, etc).", style = "color:grey"),
                          h5("Relative abundance indices may be fishery-dependent such as catch-per-unit-effort data or fishery-independent such as an annual abundance survey.", style = "color:grey"),
                          h5("In the context of annual catches and relative abundance indices, 'historical' refers to data going back to 'unfished conditions' (pre industrial fishing) such that catches may be used to reconstruct stock trajectory and indices may infer current stock depletion", style = "color:grey"),
                          h5("In contrast, 'recent' refers to data available for least 5 years from today", style = "color:grey"),
                          h5("Effort data are annual observations of fishing effort such as boat days in 2002", style = "color:grey"),
                          h5("Growth data refers to parameter estimates for growth parameters such as von Bertalanffy growth parameter K and mean asymptotic length, L-infinity.", style = "color:grey"),
                          h5("Size and age composition data are samples of size and ages in the catch going back at least 2 years from today", style = "color:grey"),
                           h5(""),
                          tagList("If you require further guidance on what types of data are available in your fishery see the DLMtool-MSC manual: ", a("Section 6", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081"))

                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==2",
                          column(7,plotOutput("plotCB",height=280)),
                          column(5,
                                h5("Under reporting of catches",style = "color:grey"),
                                h5("In some data-limited fisheries, incomplete monitoring of fishing operations may lead to under-reporting (and to a lesser extent over reporting) of annual catches",style = "color:grey"),
                                h5(""),
                                tagList("For further discussion of catch under reporting see",a("Agnew et al. (2009)", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2646833/"))
                          )

                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==3",
                          column(7,plotOutput("plotBeta",height=280)),
                          column(5,
                                h5("Is the primary index of relative abundance proportional to real biomass? Indices of relative abundance derived from fishery catch-per-unit effort may decline faster than real abundance (hyperdepletion) in cases where, for example, the species is being avoided or there has been attrition of high-density sub-population structure during early commericial fishing. Conversely catch per unit effort data may respond slower than real biomass changes if the species is being targetted, there is range contraction of fishing toward high density areas as the stock declines or the population naturally forms aggregations. For example purse-seine fisheries are often strongly hyperstable since the fish per aggregation may remain high even at low stock sizes. It may be generally assumed that a well designed fishery-independent survey is proportational to abundance but there are notable exceptions.",style = "color:grey"),
                                #h5(""),
                                #tagList("See ",a("Hilborn and Walters. (1992)", href="https://books.google.ca/books?id=Y0EGCAAAQBAJ&pg=PA190&lpg=PA190&dq=hyperstability+fisheries&source=bl&ots=v3jjRE1mwh&sig=XBbO2o7JvBqEwISAdQE83xMU5v0&hl=en&sa=X&ved=0ahUKEwiA__KW8-zZAhUJ3WMKHeL3CQ4Q6AEISjAF#v=onepage&q=hyperstability%20fisheries&f=false")),
                                tagList("See ",a("Erisman et al. (1992)", href="https://drive.google.com/open?id=1jwhIGfTmXewKWGSTNyyjoo4TefW2JiNR")),
                                tagList("or ",a("Maunder et al. (2006)", href="https://drive.google.com/open?id=1chNF72tCB_fjTjbbhZk7EyxtRU810EIc"))
                          )
                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==4",
                          #column(7,plotOutput("plotErr",height=280)),
                          column(12,
                                h5("What is the overall quality of data that is available?",style = "color:grey"),
                                h5("Perfect Information: An unrealistic and idealized observation model for testing the theoretical performance of MPs ",style = "color:grey"),
                                h5("Good quality: annual catches and abundance indices are observed with low error (<20% CV) and length/age composition data are numerous (~100 independent observations per year)",style = "color:grey"),
                                h5("Data moderate: annual catches and abundance indices are observed with greater error (<30% CV) and length/age composition data are fewer (~40 independent samples per year)",style = "color:grey"),
                                h5("Data poor: annual catches and abundance indices are imprecisely observed (<50% CV) and length/age composition data are sparse (~15 independent samples per year)",style = "color:grey"),
                                h5(""),
                                tagList("A description of the observation error model is included in ",a("Carruthers et al (2013)", href="https://drive.google.com/open?id=1EX6mu9HOb4jwlQF-Co6DQ-FJzwTcO7JP")),
                                tagList(" and a similar model was used by ",a("Carruthers et al. (2015)", href="https://drive.google.com/open?id=1xvNxp_3oUOhSaLARY_mI2cAiG2qFAgqN"))
                         )
                      )

               )

             )
        ),

        column(12,style="height:180px",
             fluidRow(

               column(width = 12,

                  conditionalPanel(condition="(input.tabs1==1 & (output.Fpanel!=0 & output.Fpanel!=undefined))|(input.tabs1==2 & (output.Mpanel!=0 & output.Mpanel!=undefined))|(input.tabs1==3 & (output.Dpanel!=0 & output.Dpanel!=undefined))",
                    HTML("<br>"),
                    textAreaInput("Justification", "", "< No reason for selection was provided >", height = "120px")
                  ),
                  conditionalPanel(condition="(input.tabs1!=4)&!((input.tabs1==1 & (output.Fpanel!=0 & output.Fpanel!=undefined))|(input.tabs1==2 & (output.Mpanel!=0 & output.Mpanel!=undefined))|(input.tabs1==3 & (output.Dpanel!=0 & output.Dpanel!=undefined)))",
                                   HTML("<br>"),
                                   textAreaInput("blank",h5("",style = "color:grey"), "", height = "120px")
                  ),
                  conditionalPanel(condition="(input.tabs1==4)",
                                   #textInput("caption", h5(""),"< No reason for selection was provided >")

                                   textAreaInput("email",h5("Alternatively email t.carruthers@oceans.ubc.ca for help:",style = "color:grey"), " From: your.email@gmail.com \n \n Hi, I've got a question about the MSC-DLMtool App...", height = "120px")
                  )




               )


            )
        ),

        column(12,style="height:80px",
            fluidRow(

               column(width = 1,
                 conditionalPanel(condition="(input.tabs1==1 & output.Fpanel>1)|(input.tabs1==2 & output.Mpanel>1)|(input.tabs1==3 & output.Dpanel>1)",
                    actionButton("Fback","< Back")
                 ),
                 conditionalPanel(condition="!((input.tabs1==1 & output.Fpanel>1)|(input.tabs1==2 & output.Mpanel>1)|(input.tabs1==3 & output.Dpanel>1))",
                                  actionButton("FbackD","< Back",style="color: #CFCFCF;  border-color: #CFCFCF") #background-color: #CFCFCF;
                 )

                ),

                column(width = 1,
                 conditionalPanel(condition="(input.tabs1==1 & output.Fpanel<14)|(input.tabs1==2 & output.Mpanel<3)|(input.tabs1==3 & output.Dpanel<4)",
                    actionButton("Fcont","Next >")
                 ),
                 conditionalPanel(condition="!((input.tabs1==1 & output.Fpanel<14)|(input.tabs1==2 & output.Mpanel<3)|(input.tabs1==3 & output.Dpanel<4))",
                                  actionButton("FcontD","Next >",style="color: #CFCFCF;  border-color: #CFCFCF") #background-color: #CFCFCF;
                 )

                ),

               column(width=2,#style="height:180px",
                      conditionalPanel(condition="output.Fpanel>0|output.Ppanel>0|output.Dpanel>0|output.Fpanel!=undefined|output.Mpanel!=undefined|output.Dpanel!=undefined",
                                       #h4("Progress",style="color:grey"),
                                       #hr(),
                                       textOutput("Fpanelout"),
                                       textOutput("Mpanelout"),
                                       textOutput("Dpanelout")

                      )
               ),
               column(4,offset=1,style="height:80px",

                      #conditionalPanel(condition="output.Fpanel==0 & output.Mpanel==0 & output.Dpanel==0",
                       #                actionButton("start",h5("START",style="color:green"),width=300)
                      #),
                      #conditionalPanel(condition="!(output.Fpanel==0 & output.Mpanel==0 & output.Dpanel==0)",
                                       actionButton("Calculate",h5("CALCULATE",style="color:red"),width=300)
                      #)

               ),
               column(width = 1,offset=2,
                      conditionalPanel(condition="input.tabs1==4",
                            actionButton("email","Send")
                      )

               )

             )
        ),


        conditionalPanel(condition="input.Calculate>0",
          column(12,
              h4("Results",style = "color:black"),
              hr(),
              conditionalPanel(condition="input.Calculate>0",
                fluidRow(
                column(width = 12,

                   h5("Performance Indicator Table",style="color::grey"),
                   tableOutput('Ptable')
                   #img(src = "Ranking table.jpg", height = 260, width = 550)

                ),
                column(width = 6,

                   h5("PI.1.1.1a vs Long term yield trade-off",style="color::grey"),
                   plotOutput("P1_LTY")
                   #img(src = "Results1.jpg", height = 600, width = 600)

                ),
                column(width = 12),
                column(width = 12,

                   h5("B/BMSY and F/FMSY projection plots",style="color::grey"),
                   plotOutput("wormplot")
                       #img(src = "Results1.jpg", height = 600, width = 600)

                ),
                column(width = 12),
                column(width = 12,

                       h5("HCR and Tools, PI.1.2.2.a",style="color::grey"),
                       plotOutput("HCR")
                       #img(src = "Results1.jpg", height = 600, width = 600)

                ),
                column(width = 12),
                column(12,
                       HTML("<br>"),
                       h5("Cost of Current Uncertainties Analysis",style = "color:black"),
                       plotOutput("CCU",height=650)
                )

               )
            )
          ),

          column(12,style="height:80px",
               HTML("<br>"),
               h4("Ancillary Indicators",style = "color:black"),
               hr()
          )

        ),

        column(12,style="height:40px",
             h4("Reporting",style = "color:black"),
             hr()
        ),

        column(3,style="height:50px",
             HTML("<br>"),
             downloadButton("Build_OM","Build operating model report")
        ),

        conditionalPanel(condition="input.Calculate>0",
          column(3,style="height:50px",
                 HTML("<br>"),
                 downloadButton("Build_Eval","Build evaluation report")
          ),

          column(3,style="height:50px",
                 HTML("<br>"),
                 downloadButton("Build_VOI","Build value of information report")
          ),

          column(3,style="height:50px",
                 HTML("<br>"),
                 downloadButton("Build_AI","Build ancillary indicators report")
          )


        ),

        column(12,style="height:30px"),

        column(12,style="height:160px",
             h4("File",style = "color:black"),
             hr(),
             column(3,style="padding:10px",
                                    fileInput("Load","Load a previous session")),

             column(3,
                    h5("Save progress",style="font-weight:bold"),
                    downloadButton("Save","",width=70))
            # column(8,textInput("File", "File name:", "myStock.msc"))
        ),

        column(10,style="height:80px"),

        column(2,style="height:80px",
               #h4(paste("<",getSessionID(),">"),style="color:grey")
               textOutput("SessionID")
        ),
        column(12,style="height:60px"),


        column(12,style="height:100px",
               hr(),
               h4("Extended Options",style = "color:black"),
            hr()
        ),

        column(12,
             radioButtons("Analysis_type","Analysis type:", c("Demo"="Demo","Risk Evaluation" = "RE", "FIP" = "FIP", "Certification" = "Cert"), selected="Demo",inline=T)
        ),
        column(12),
        column(3,style="height:80px;padding:18px;",
             checkboxInput("Ref_MPs", label = "Include reference MPs", value = FALSE)
        ),
        column(3,style="height:80px;padding:18px;",
             checkboxInput("Parallel", label = "Run with parallel computation (no progress bar)", value = FALSE)
        ),
        column(12),
        column(3,style="height:90px;padding:18px;",
             numericInput("interval", label = "Management update interval (years)", value=4)
        ),
        column(3,style="height:90px;padding:18px;",
             numericInput("proyears", label = "Number of projected years for the simulation", value=50)
        ),
        column(3,style="height:90px;padding:18px;",
             numericInput("nsim", label = "Number of simulations", value=16)
        ),
        column(12),
        column(6, textInput("WD", "Select a working directory for this session", "C:/myprojects/risk_assessment/swordfish")),
        column(6, textInput("Source", "Source custom DLMtool/MSEtool MPs, performance metrics, MSE controls", "mysource.r")),

        column(12,style="height:60px"),
        column(6, textInput("Debug1", "Debug window", ""))

     ) # end of fluid row
    ) # end of fluid page
  ) # end of server





