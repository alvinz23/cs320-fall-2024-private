

type tree =
  | Leaf of int
  | Node of tree list

  let rec combineLists listOfLists =
    match listOfLists with
    | [] -> []
    | h :: t -> h @ combineLists t
  
    let rec gatherLeaves treeNode =
      match treeNode with
      | Leaf _ -> [treeNode]
      | Node [] -> [treeNode]
      | Node childrenTrees -> combineLists (List.map gatherLeaves childrenTrees)
    

let collapse height treeStructure =
  let rec solve currHeight treeNode =
    match treeNode with
    | Leaf _ -> treeNode
    | Node childrenNodes ->
      if currHeight = height - 1 then
        let leafNodes = combineLists (List.map gatherLeaves childrenNodes) in
        Node leafNodes
      else
        let collapsedChildren = List.map (solve (currHeight + 1)) childrenNodes in
        Node collapsedChildren
  in
   solve 0 treeStructure
