#' @export
plt_gubbin_tree = function(gubbin_tree){
  plt_tree = plot(gpsc8_tree, "c",FALSE, cex=0.6, no.margin = TRUE)
  return(plt_tree)
}

#' @export
match_tree_meta = function(Metadata, Tree, drop_tip_name, drop_tip = TRUE){
  if (drop_tip){
    ## drop reference tip from Gubbin tree
    tree_dropRef = drop.tip(Tree,drop_tip_name)
    ## create vector of tip labels
    id_order = tree_dropRef$tip.label
  } else{
    tree_dropRef = Tree
    id_order = Tree$tip.label
  }
  ## reorder tree based on order of tip labels (the order of the tip label should be the same in the lane_id)
  match_order = Metadata %>%
    dplyr::slice(match(id_order,lane_id))
  
  res_list = list("tree" = tree_dropRef, "match_tree_meta" = match_order)
  return(res_list)
}

#' @export
time_resolved = function(match_tree_meta, dropRef_tree, save_bd_path,time="Year_collection"){
  collectionTime = as.numeric(match_tree_meta$time)
  rtd = roottotip(dropRef_tree, collectionTime)
  bd = bactdate(unroot(dropRef_tree), collectionTime, showProgress = T) ## MCMC process
  saveRDS(bd, save_bd_path)
  
  res_list = list("Root2Tip" = rtd, "BactDating" = bd)
  return(res_list)
}
