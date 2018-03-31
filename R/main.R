#' @include utils.main.R
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

#' Aligns the tilder '~' within a highlighted area.
#'
#' @return Aligns the tilde (\code{~}) within a highlighted region.
#' @export
alignTilde <- function() {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find = "~", where = area)
  insertList <- assembleInsert(loc)
  insertr(insertList, capture$id)
}


#' Align a given regular expression.
#'
#' @param x Custom input; a regular expression.
#' @param ... Additional arguments passed along to \code{regexec()}
#' @return Aligns regular expression within a highlighted region.
#' @export
alignRegex <- function(x, ...) {
  capture <- capture()
  area    <- captureArea(capture)
  loc     <- findRegEx(find  = x,
                       where = area,
                       ...)
  insertList <- assembleInsert(loc)
  insertr(insertList)
}


#' Align a given regular expression.
#'
#' @return Aligns regular expression within a highlighted region.
#' @export
alignCustom <- function() {
  ui <- miniUI::miniPage(
    shiny::includeCSS(system.file("gadget/css/app.css", package = "AlignRegex")),
    miniUI::gadgetTitleBar("Align Regular Expressions of Your Choice", left = NULL),
    miniUI::miniContentPanel(
      shiny::h2("Pre-set Regular Expressions"),
      miniUI::miniButtonBlock(
        shiny::actionButton("alignAssign" , "<-"),
        shiny::actionButton("alignAssign2", "="),
        shiny::actionButton("alignTilde"  , "~"),
        shiny::actionButton("alignColon"  , ":"),
        shiny::actionButton("alignAS"     , "AS")
        ),
      shiny::hr(),
      shiny::h2("Custom Regular Expression"),
      shiny::textInput("regex", NULL),
      shiny::fillRow(
        shiny::actionButton("runRegex", "Run"),
        shiny::checkboxInput("perl", "perl", TRUE),
        shiny::checkboxInput("ingore.case", "ignore.case", FALSE),
        width = "35%", height = "10%"
        )
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
    # Align =
    shiny::observeEvent(input$alignTilde, {
      alignTilde()
      invisible(shiny::stopApp())
    })
    # Align :
    shiny::observeEvent(input$alignColon, {
      alignRegex(":")
      invisible(shiny::stopApp())
    })
    # Align AS
    shiny::observeEvent(input$alignAS, {
      alignRegex("AS")
      invisible(shiny::stopApp())
    })
    # Align Regex
    # Handle checkboxes first
    # txt <- reactiveValues(input = NULL)
    shiny::observeEvent(input$runRegex, {
      # Don't run if input value was not given
      if(input$regex == "") invisible(shiny::stopApp())
      alignRegex(input$regex, perl = input$perl, ignore.case = input$ignore.case)
      invisible(shiny::stopApp())
    })

    shiny::observeEvent(input$done, {
      invisible(shiny::stopApp())
    })


}
  viewer <- shiny::dialogViewer("AlignRegex")
  shiny::runGadget(ui, server, viewer = viewer)
}
