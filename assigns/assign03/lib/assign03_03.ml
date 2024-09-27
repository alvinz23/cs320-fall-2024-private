

type tree =
  | Leaf of int
  | Node of tree list

let collapse height treeStructure =
  let rec combineLists listOfLists =
    match listOfLists with
    | [] -> []
    | headList :: tailLists -> headList @ combineLists tailLists
  in
  let rec gatherLeaves treeNode =
    match treeNode with
    | Leaf _ -> [treeNode]
    | Node [] -> [treeNode]
    | Node childrenTrees -> combineLists (List.map gatherLeaves childrenTrees)
  in
  let rec collapseTreeAtHeight currentTreeHeight treeNode =
    match treeNode with
    | Leaf _ -> treeNode
    | Node childrenNodes ->
      if currentTreeHeight = height - 1 then
        let leafNodes = combineLists (List.map gatherLeaves childrenNodes) in
        Node leafNodes
      else
        let collapsedChildren = List.map (collapseTreeAtHeight (currentTreeHeight + 1)) childrenNodes in
        Node collapsedChildren
  in
   collapseTreeAtHeight 0 treeStructure
