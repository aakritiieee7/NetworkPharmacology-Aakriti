#' Network interaction diagram of herbs, ingredients, and targets
#'
#' @param network.data data frame
#' must contain herb, molecule, target three columns of data
#' @param node.color node color
#' see "RColorBrewer::display.brewer.all()"
#' @param node.size  node size
#' @param label.size label size
#' @param label.degree
#' the node degree is the number of connections that
#' the node has with the other nodes.
#' Nodes with connections greater than or
#' equal to degree will be displayed.
#' @param edge.color edge color
#' @param edge.width edge width
#' @param graph.layout etwork Diagram Layout:
#' @param graph.layout Network Diagram Layout:
#' "kk", "nicely", "circle", "sphere",
#' "bipartite", "star", "tree", "randomly",
#' "gem", "graphopt","lgl", "grid",
#' "mds", "sugiyama","fr"
#' @param rem.dis.inter remove single free unconnected nodes
#' @param label.repel label repel
#'
#' @return Network Diagram
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 aes
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph scale_edge_width
#' @importFrom ggraph geom_edge_link0
#' @importFrom ggraph theme_graph
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph graph_from_data_frame
#' @examples
#' \dontrun{
#' data("xfbdf", package = "TCMNP")
#' network.data <- xfbdf %>%
#'   dplyr::select(herb, molecule, target) %>%
#'   sample_n(100, replace = FALSE) %>%
#'   as.data.frame()
#' tcm_net(network.data, node.color= "Spectral",
#' label.degree = 0, rem.dis.inter = TRUE,graph.layout = "fr",
#' label.size = 3)
#' }
tcm_net <- function(network.data,
                    node.color = "RdBu",
                    node.size = c(2, 8),
                    label.size = 4,
                    label.degree = 3,
                    label.repel = TRUE,
                    edge.color = "lightgrey",
                    edge.width = c(0.2, 2),
                    graph.layout = "kk",
                    rem.dis.inter = FALSE) {

  # Convert input to data frame
  network.data <- as.data.frame(network.data)

  # Ensure columns are character type
  network.data$herb <- as.character(network.data$herb)
  network.data$molecule <- as.character(network.data$molecule)
  network.data$target <- as.character(network.data$target)

  # Trim hidden spaces from all three columns
  network.data <- network.data %>%
    mutate(
      molecule = stringr::str_trim(molecule),
      herb = stringr::str_trim(herb),
      target = stringr::str_trim(target)
    )

  # Remove duplicates AFTER cleaning
  network.data <- unique(network.data)

  # Create network links
  links <- rbind(
    network.data %>%
      dplyr::select(herb, molecule) %>%
      dplyr::rename(from = herb, to = molecule) %>%
      dplyr::mutate(weight = 1),

    network.data %>%
      dplyr::select(molecule, target) %>%
      dplyr::rename(from = molecule, to = target) %>%
      dplyr::mutate(weight = 1)
  ) %>%
    dplyr::distinct()

  # Create nodes list correctly (FIXED)
  nodes <- data.frame(
    node = unique(c(links$from, links$to)),
    stringsAsFactors = FALSE
  )

  # Build graph from data
  net <- igraph::graph_from_data_frame(
    d = links,
    vertices = nodes,
    directed = FALSE
  )

  # Assign node degrees
  igraph::V(net)$degree <- igraph::degree(net)

  # ---- CRITICAL FIX: Proper Class Assignment ----
  node_vec <- as.character(nodes$node)

  igraph::V(net)$class <- ifelse(
    node_vec %in% network.data$herb, "herb",
    ifelse(node_vec %in% network.data$molecule, "molecule", "target")
  )

  # Node size based on degree
  igraph::V(net)$size <- igraph::degree(net)

  # Edge score
  igraph::E(net)$score <- igraph::E(net)$weight

  # ---- FINAL PLOTTING (Readable + Logical Shapes) ----
  ggraph::ggraph(net, layout = graph.layout) +
    geom_edge_link0(aes(edge_linewidth = weight),
                    edge_colour = edge.color) +

    geom_node_point(aes(
      color = degree,
      size = degree,
      shape = class
    ), alpha = 1.0) +

    ggplot2::scale_color_gradientn(colours = RColorBrewer::brewer.pal(8, name = node.color)) +
    geom_node_text(aes(
      filter = degree >= label.degree,
      label = name
    ), size = label.size, repel = label.repel) +

    scale_edge_width(range = edge.width) +
    scale_size_continuous(name = "degree",
                          range = node.size) +
    theme_graph()
}
