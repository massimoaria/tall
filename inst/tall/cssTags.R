cssTags <- function() {
  tagList(
    ## Team Card Style
    tags$head(
      tags$style(HTML(
        "
    .modal-lg {
      max-width: 900px;
    }

    .modal-dialog {
      margin-top: 40px;
    }

    .modal-content {
      border-radius: 15px;
      border: none;
      box-shadow: 0 10px 40px rgba(0,0,0,0.2);
    }

    .modal-header {
      border-bottom: none;
      padding: 25px 25px 10px 25px;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      border-radius: 15px 15px 0 0;
    }

    .modal-header .close {
      color: white;
      opacity: 0.8;
    }

    .modal-header span {
      color: white !important;
    }

    .modal-body {
      padding: 10px 25px 25px 25px;
      background: #ffffff;
    }

    .modal-footer {
      border-top: none;
      padding: 15px 25px;
    }
  "
      ))
    ),

    ## workaround to solve visualization issues in Data Table
    tags$head(tags$style(HTML(
      ".has-feedback .form-control { padding-right: 0px;}"
    ))),
    ## script to open more times the same modal ####
    tags$script(
      "
    Shiny.addCustomMessageHandler('button_id', function(value) {
    Shiny.setInputValue('button_id', value);
    });
  "
    ),
    tags$script(
      "
    Shiny.addCustomMessageHandler('button_id2', function(value) {
    Shiny.setInputValue('button_id2', value);
    });
  "
    ),
    tags$script(
      "
    Shiny.addCustomMessageHandler('click', function(value) {
    Shiny.setInputValue('click', value);
    });
  "
    ),
    tags$script(
      "
    Shiny.addCustomMessageHandler('click_dend', function(value) {
    Shiny.setInputValue('click_dend', value);
    });
  "
    ),
    #### DROPDOWN MENUS STYLE ###############
    tags$head(
      tags$style(HTML(
        "
    /* Collapsible Options Panel Styles - Biblioshiny inspired */
    .options-panel {
      background: white;
      border-radius: 8px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      padding: 15px;
      width: 320px;
    }

    .options-header {
      font-size: 16px;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 15px;
      padding-bottom: 10px;
      border-bottom: 2px solid #e0e0e0;
    }

    /* Configuration Section */
    .config-section {
      background: #f8f9fa;
      border-left: 4px solid #4F7942;
      padding: 12px;
      margin-bottom: 12px;
      border-radius: 4px;
    }

    .config-section-header {
      font-weight: 600;
      color: #4F7942;
      font-size: 14px;
      margin-bottom: 10px;
      display: flex;
      align-items: center;
      gap: 8px;
    }

    /* Filter Section - Yellow theme like Stop Words */
    .filter-section {
      background: #fff9e6;
      border-left: 4px solid #FFA800;
      padding: 12px;
      margin-bottom: 12px;
      border-radius: 4px;
    }

    .filter-section-header {
      font-weight: 600;
      color: #d68910;
      font-size: 14px;
      margin-bottom: 10px;
      display: flex;
      align-items: center;
      gap: 8px;
    }

    /* Advanced Section - Green theme like Synonyms */
    .advanced-section {
      background: #e8f5e9;
      border-left: 4px solid #66bb6a;
      padding: 12px;
      margin-bottom: 12px;
      border-radius: 4px;
    }

    .advanced-section-header {
      font-weight: 600;
      color: #388e3c;
      font-size: 14px;
      margin-bottom: 10px;
      display: flex;
      align-items: center;
      gap: 8px;
    }

    /* Parameters Section - Gray theme */
    .params-section {
      background: #f5f5f5;
      border-left: 4px solid #757575;
      padding: 12px;
      margin-bottom: 12px;
      border-radius: 4px;
    }

    .params-section-header {
      font-weight: 600;
      color: #424242;
      font-size: 14px;
      margin-bottom: 10px;
      display: flex;
      align-items: center;
      gap: 8px;
    }

    /* Collapse toggle button */
    .section-toggle {
      cursor: pointer;
      user-select: none;
      transition: all 0.3s ease;
    }

    .section-toggle:hover {
      opacity: 0.8;
    }

    .section-toggle-icon {
      float: right;
      transition: transform 0.3s ease;
    }

    .section-toggle-icon.open {
      transform: rotate(180deg);
    }

    /* Input styling within sections */
    .options-panel .form-group {
      margin-bottom: 12px;
    }

    .options-panel .control-label {
      font-size: 13px;
      font-weight: 500;
      color: #555;
      margin-bottom: 5px;
    }

    .options-panel .form-control {
      border-radius: 4px;
      border: 1px solid #ddd;
      font-size: 13px;
    }

    .options-panel .form-control:focus {
      border-color: #4F7942;
      box-shadow: 0 0 0 0.2rem rgba(79, 121, 66, 0.25);
    }
  "
      ))
    ),
    # CSS PERSONALIZZATO PER VALUEBOX COLORATE E COMPATTE
    tags$head(
      tags$style(HTML(
        "
    /* Corpus Size & Structure - Green shades */
    #clickbox1 .small-box { background-color: #5cb85c !important; color: white !important; }
    #clickbox4 .small-box { background-color: #5cb85c !important; color: white !important; }
    #clickbox8 .small-box { background-color: #5cb85c !important; color: white !important; }
    #clickbox7 .small-box { background-color: #5cb85c !important; color: white !important; }
    #clickbox9 .small-box { background-color: #5cb85c !important; color: white !important; }

    /* Average Length Metrics - Blue/Teal shades */
    #clickbox2 .small-box { background-color: #17a2b8 !important; color: white !important; }
    #clickbox3 .small-box { background-color: #17a2b8 !important; color: white !important; }
    #clickbox5 .small-box { background-color: #17a2b8 !important; color: white !important; }
    #clickbox6 .small-box { background-color: #17a2b8 !important; color: white !important; }

    /* Lexical Metrics - Orange shades */
    #clickbox10 .small-box { background-color: #f39c12 !important; color: white !important; }
    #clickbox11 .small-box { background-color: #f39c12 !important; color: white !important; }
    #clickbox12 .small-box { background-color: #f39c12 !important; color: white !important; }
    #clickbox13 .small-box { background-color: #f39c12 !important; color: white !important; }
    #clickbox14 .small-box { background-color: #f39c12 !important; color: white !important; }
    #clickbox15 .small-box { background-color: #f39c12 !important; color: white !important; }
    #clickbox16 .small-box { background-color: #f39c12 !important; color: white !important; }

    /* Riduzione dimensioni e padding valueBox */
    .small-box {
      border-radius: 8px !important;
      padding: 15px !important;
      min-height: 100px !important;
      height: 100px !important;
      margin-bottom: 10px !important;
    }

    .small-box h3 {
      font-size: 32px !important;
      font-weight: bold !important;
      margin: 5px 0 !important;
      color: white !important;
    }

    .small-box p {
      font-size: 14px !important;
      margin: 0 !important;
      color: white !important;
      font-weight: 500 !important;
    }

    /* Icone ridotte e bianche con opacità - RIDOTTE DA fa-3x */
    .small-box .icon-large {
      font-size: 20px !important;
      color: rgba(255, 255, 255, 0.25) !important;
      position: absolute !important;
      right: 10px !important;
      top: 50% !important;
      transform: translateY(-50%) !important;
    }

    /* Group headers styling */
    .group-header {
      font-size: 18px;
      font-weight: bold;
      margin-bottom: 15px;
      padding-bottom: 8px;
      padding-left: 5px;
      border-bottom: 3px solid;
    }

    .group-header-green {
      border-bottom-color: #5cb85c !important;
      color: #5cb85c !important;
    }

    .group-header-blue {
      border-bottom-color: #17a2b8 !important;
      color: #17a2b8 !important;
    }

    .group-header-orange {
      border-bottom-color: #f39c12 !important;
      color: #f39c12 !important;
    }

    /* Rimuovi margini extra dalle colonne */
    .overview-row {
      margin-left: 0 !important;
      margin-right: 0 !important;
    }

    .overview-row > div {
      padding-left: 5px !important;
      padding-right: 5px !important;
    }

    /* Flex container per 7 box sulla stessa riga */
    .lexical-row {
      display: flex !important;
      flex-wrap: nowrap !important;
      gap: 10px !important;
      margin-left: 0 !important;
      margin-right: 0 !important;
    }

    .lexical-row > div {
      flex: 1 !important;
      min-width: 0 !important;
      padding: 0 5px !important;
    }
  "
      ))
    ),

    tags$style(HTML(
      "
  /* Details/Summary styling for collapsible sections */
  details {
    margin-bottom: 12px;
  }

  details summary {
    list-style: none;
    cursor: pointer;
  }

  details summary::-webkit-details-marker {
    display: none;
  }

  details[open] summary {
    margin-bottom: 10px;
  }
"
    )),
    #### BUTTON STYLE ###############
    tags$style(
      ".glyphicon-refresh {color:#ffffff; font-size: 15px; align: center;}"
    ),
    tags$style(
      ".fa-magnifying-glass {color:#ffffff; font-size: 15px; align: center;}"
    ),
    tags$style(
      ".fa-microchip {color:#ffffff; font-size: 15px; align: center;}"
    ),
    tags$style(
      ".glyphicon-download-alt {color:#ffffff; font-size: 18px; align: center; margin-left: -3.5px}"
    ),
    tags$style(
      ".glyphicon-play {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"
    ),
    tags$style(
      ".glyphicon-remove {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"
    ),
    tags$style(
      ".glyphicon-search {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"
    ),
    tags$style(
      ".glyphicon-repeat {color:#ffffff; font-size: 18px; align: center;margin-left: -3px; padding-left: -15px}"
    ),
    tags$style(
      ".glyphicon-plus {color:#ffffff; font-size: 18px;align: center; margin-left: -2px}"
    ),
    tags$style(
      ".glyphicon-cog {color:#4F794290; font-size: 21px; margin-top: 2.3px; margin-left: -3px}"
    ),
    tags$style(
      ".fa-sliders {color:#4F794290; font-size: 21px; margin-top: 2.3px; margin-left: -3px}"
    ),
    tags$style(
      ".glyphicon-floppy-save {color:#ffffff; font-size: 18px; text-align:center; padding-right: -10px;
             margin-top: 1px;}"
    ), # margin-top: 4px; margin-down: 22px; margin-right: 25px}"),
    tags$style(
      ".glyphicon-download {color:#ffffff; font-size: 18px; align: center;margin-top: 3px}"
    ),
    tags$style(".glyphicon-folder-open {color:#ffffff; font-size: 17px}"),
    tags$head(
      tags$style("mark {background-color: #6CC283;}"), ## Color for highlighted text #5a918a

      tags$style(".fa-envelope {color:#FF0000; font-size: 20px}"),
      tags$style(".fa-envelope-open {font-size: 20px}"),
      tags$style(".fa-cube {font-size: 20px}"),
      tags$style(".fa-question {font-size: 20px}"),
      tags$style(".fa-comment-dollar {font-size: 20px}"),
      tags$style(".fa-bars {font-size: 20px}"),
      tags$style(".sidebar-toggle {font-size: 15px}"),
      tags$style(".fa-users {font-size: 18px}"),

      ## radio button color for primary status (Lemma or Token)
      tags$style(HTML(
        "
    /* Change the default primary button color to a gradient */
    .btn-primary {
      background: linear-gradient(to right,rgb(191, 191, 191),rgb(148, 148, 148),rgb(123, 123, 123)) !important;
      border: none !important;
      color: white !important;
    }

    /* Change the hover state */
    .btn-primary:hover,
    .btn-primary.hover {
      background: linear-gradient(to left, #4F7942,rgb(62, 97, 52)) !important;
      border: none !important;
    }

    /* Change the active state */
    .btn-primary:active,
    .btn-primary.active {
      background: linear-gradient(to right, #95D297, #6CC283, #4F7942) !important;
      border: none !important;
    }

    /* Make button labels bold */
    .btn-group .btn {
      font-weight: bold;
    }
  "
      ))
    ),
    tags$head(
      tags$style(HTML(
        "

     .multicol {

       -webkit-column-count: 2; /* Chrome, Safari, Opera */

       -moz-column-count: 2; /* Firefox */

       column-count: 2;

     }

   "
      ))
    ),

    ## Style for selectInput menu with several choices
    tags$head(
      tags$style(HTML(
        "
      .selectize-dropdown-content {
        max-height: 200px !important; /* Limita l'altezza della lista */
        overflow-y: auto !important; /* Aggiunge lo scrolling verticale */
        overflow-x: auto !important; /* Aggiunge lo scrolling orizzontale se necessario */
        white-space: nowrap !important; /* Evita il wrapping del testo */
      }
    "
      ))
    )
  )
}
