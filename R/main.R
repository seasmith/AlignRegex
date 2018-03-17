#' @include utils.main.R
#' @include utils.R
NULL

#' Align a highlighted region's assignment operators.
#'
#' @return
#' Aligns the single caret operators (\code{<-}) within a highlighted region.
#' @export
alignAssign <- function() {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find = "<-", where = area)
  insertList <- assembleInsert(loc)
  insertr(insertList, capture$id)
}



#' Align a highlighted region's assignment operators.
#'
#' @return Aligns the equal sign assignment operators (\code{=}) within a
#' highlighted region.
#' @export
alignAssign2 <- function() {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find = "=", where = area)
  insertList <- assembleInsert(loc)
  insertr(insertList, capture$id)
}



#' Align a highlighted region's AS clauses within a SQL statement.
#'
#' @return Aligns the AS clauses in a SQL statement within a highlighted region.
#' @export
alignAS <- function() {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find        = "as",
                       where       = area,
                       ignore_case = TRUE,
                       wrap_space  = TRUE)
  insertList <- assembleInsert(loc)
  insertr(insertList, capture$id)
}



#' Align a highlighted region's : separators.
#'
#' @return Aligns the : delimiter found in YAML within a highlighted region.
#' @export
alignColon <- function() {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find        = ":",
                       where       = area)
  insertList <- assembleInsert(loc)
  insertr(insertList)
}


#' Align a given regular expression.
#'
#' @param x Custom input; a regular expression.
#' @return Aligns regular expression within a highlighted region.
#' @export
alignRegex <- function(x) {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find        = x,
                       where       = area)
  insertList <- assembleInsert(loc)
  insertr(insertList)
}


#' Align a given regular expression.
#'
#' @return Aligns regular expression within a highlighted region.
#' @export
alignCustom <- function() {
  ui <- miniUI::miniPage(
    includeHighlightJs(),
    miniUI::gadgetTitleBar("Align Your Regular Expressions", left = NULL),
    miniUI::miniContentPanel(
      shiny::strong("Pre-set RegEx's to match and align."),
      # shiny::hr(),
      miniUI::miniButtonBlock(
        shiny::actionButton("alignAssign", "<-"),
        shiny::actionButton("alignAssign2", "="),
        shiny::actionButton("alignColon", ":"),
        shiny::actionButton("alignAS", "AS")
      ),
      shiny::hr(),
      shiny::textInput("regex", "Type your regular expression here"),
      shiny::actionButton("runRegex", "Run")
    )
  )

  server <- function(input, output, session) {

    # Align <-
    shiny::observeEvent(input$alignAssign, {
      alignAssign()
      invisible(shiny::stopApp())
    })
    # Align =
    shiny::observeEvent(input$alignAssign2, {
      alignAssign2()
      invisible(shiny::stopApp())
    })
    # Align :
    shiny::observeEvent(input$alignColon, {
      alignColon()
      invisible(shiny::stopApp())
    })
    # Align AS
    shiny::observeEvent(input$alignAS, {
      alignAS()
      invisible(shiny::stopApp())
    })
    # Align Regex
    # txt <- reactiveValues(input = NULL)
    shiny::observeEvent(input$runRegex, {
      # Don't run if input value was not given
      if (input$regex == "") invisible(shiny::stopApp())
      alignRegex(input$regex)
      invisible(shiny::stopApp())
    })

    shiny::observeEvent(input$done, {
      invisible(shiny::stopApp())
    })


}
  viewer <- shiny::dialogViewer("AlignRegex")
  shiny::runGadget(ui, server, viewer = viewer)
}
