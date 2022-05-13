title <- tags$div(HTML('<table text-align=left cellspacing=-10 cellPadding=30>
    <tr><th rowspan = 2>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <a id = "logo" href = "http://www.inm.gov.co" title = "Masscor Graphical User Interface" data-height="80">
    <img src = "INM_masscor.png" height = "90" alt = "INM de Colombia" style = "margin-top: 5px">
    </a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    </th>
    <th><h1 style="LINE-HEIGHT:5px; color: #dddddd; margin-bottom: 5px; font-size:45px;"><b>masscor package</b></h1></th></tr>
    <tr><th><h3 style="LINE-HEIGHT:0px; color: #dddddd; margin-top: 4px;">
    Graphical User Interface
    </h3></th></tr>
    </table>'))

headTags <- tags$li(
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(
    # Navigation Bar:
    tags$style(HTML(' .navbar {
                          height: 100px;
                          min-height:100px !important;
                        }
                      .navbar-nav > li > a, .navbar-brand {
                            padding-top:1px !important; 
                            padding-bottom:1px !important;
                            height: 100px;
                      }
                      .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {
                            color: #dddddd;
                            background-color: #1a242f75;
                            font-size:15px;
                            font-weight: bold;
                            vertical-align: bottom;
                            display: table-cell;
                      }
                      .navbar-default .navbar-nav>li>a {
                            color: #9f9f9f;
                            font-size:15px;
                            font-weight: bold;
                            vertical-align: bottom;
                            display: table-cell;
                      }
                      .navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {
                            color: #ffffff;
                            background-color: transparent;
                      }
                      .navbar-brand {
                            float: left;
                            padding: 19.5px 55px 10px 10px;
                            font-size: 19px;
                            line-height: 21px;
                            height: 60px;
                      }')),
    tags$style(type = "text/css", "body {padding-top: 60px;     
                background-color: #efefef;}"),
    
    # TabBoxes
    tags$style(HTML('.nav-tabs {
                            border-bottom: 2px solid #ffffff;
                    }
                    .tab-content>.active {
                           display: block;
                           background-color: #ffffff;
                    }
                    
                    .nav-tabs>li.active>a, .nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus {
                            color: #1a242f;
                            background-color: #ffffff;
                            border: 2px solid #ffffff;
                            border-bottom-color: transparent;
                            cursor: default;
                            font-weight: bold;
                    }
                    .nav-tabs>li>a {
                            background-color: #e1e1e1;
                            margin-right: 2px;
                            line-height: 1.42857143;
                            border-radius: 4px 4px 0 0;
                    }
                    a {
                            color: #2c3e50;
                            text-decoration: none;
                    }
                    a:hover, a:focus {
                            color: #1a242f;
                            text-decoration: underline;
                    }')),
    
    tags$style(HTML('.shiny-notification {position:fixed; top: calc(50% - 150px); left: calc(50% - 150px); 
                                         height: auto !important; opacity:0.98; margin-right:500px}
                     .btn-box-tool {color: #001848; font-size: 15px}')),
    tags$style(type = "text/css", "#inline label{ display: table-cell; text-align: right; vertical-align: middle; } 
               #inline .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineTOP label{ display: table-cell; text-align: right; vertical-align: top; } 
               #inlineTOP .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineBOT label{ display: table-cell; text-align: right; vertical-align: bottom; } 
               #inlineBOT .form-group {display: table-row;}")
    )
  )