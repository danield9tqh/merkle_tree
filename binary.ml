type 'a binary_tree =
  | Leaf
  | Node of 'a node
  [@@deriving show]
and 'a node =
  {size: int; left: 'a binary_tree; value: 'a; right: 'a binary_tree}
  [@@deriving show]

type binary_merkle_tree = string node
[@@deriving show]

type binary_merkle_proof ={ value: string; steps: binary_merkle_proof_step list }
and binary_merkle_proof_step =
  | Left of string
  | Right of string

let to_merkle_node (hash: string -> string) (value: string): binary_merkle_tree =
  {size= 1; left= Leaf; value= hash value; right= Leaf}

let merge (hash: string -> string) (left: binary_merkle_tree) (right: binary_merkle_tree): binary_merkle_tree =
  {size= left.size + right.size; left= Node left; value=hash (left.value^right.value); right= Node right}

type 'a ne_list = 'a * 'a list

let rec pair_off (hash: string -> string) (values: binary_merkle_tree ne_list): binary_merkle_tree ne_list =
  match values with
  | (n1, []) -> (n1, [])
  | (n1, [n2]) -> (merge hash n1 n2, [])
  | (n1, n2::n3::ns) ->
    let (p, ps) = pair_off hash (n3, ns) in
      (merge hash n1 n2, p::ps)

let rec aggregate_by_pairs (hash: string -> string) (values: binary_merkle_tree ne_list): binary_merkle_tree =
  let paired = pair_off hash values in
  match paired with
  | (n1, []) -> n1
  | _ -> aggregate_by_pairs hash (pair_off hash paired)

let build_merkle_tree (hash: string -> string) (values: string ne_list): binary_merkle_tree =
  let (v, vs) = values in
  let merkleized_values = ((to_merkle_node hash v), List.map (to_merkle_node hash) vs) in
  aggregate_by_pairs hash merkleized_values

let log2 x = Float.div (Float.log x) (Float.log 2.)
let power_of_2 x = let l2 = (log2 (Int.to_float x)) in Float.equal (Float.floor l2) l2

let size node = match node with
| Leaf -> 0
| Node n -> n.size

let rec insert (hash: string -> string) (value: string) (root: binary_merkle_tree): binary_merkle_tree =
  let is_full = power_of_2 root.size in
  if is_full then
    let new_right = to_merkle_node hash value in
    let new_left = root in
    merge hash new_left new_right
  else
    match (root.left, root.right) with
    | (_, Leaf) ->
      (let new_left = root in
      let new_right = to_merkle_node hash value in
      merge hash new_left new_right)
    | (Leaf, _) ->
      (let new_left = to_merkle_node hash value in
      let new_right = root in
      merge hash new_left new_right)
    | (Node l, Node r) -> if l.size >= r.size then
      (let new_left = l in
      let new_right = insert hash value r in
      merge hash new_left new_right) else
      (let new_left = insert hash value l in
      let new_right = r in
      merge hash new_left new_right)

let rec contruct_root_hash_from_proof (hash: string -> string) (proof: binary_merkle_proof): string = 
match proof.steps with
| [] -> hash proof.value
| (x::xs) -> match x with
  | Left value -> hash (value^(contruct_root_hash_from_proof hash {value=proof.value; steps=xs}))
  | Right value -> hash ((contruct_root_hash_from_proof hash {value=proof.value; steps=xs})^value)

let check_proof (hash: string -> string) (proof: binary_merkle_proof) (root_hash: string): bool = 
  String.equal (contruct_root_hash_from_proof hash proof) root_hash

let add_proof_step (subproofs: binary_merkle_proof list) (step: binary_merkle_proof_step): binary_merkle_proof list =
  List.map (fun sp -> {value=sp.value; steps=step::sp.steps}) subproofs

let rec create_proofs (hash: string -> string) (value: string) (root: binary_merkle_tree): binary_merkle_proof list =
  match (root.left, root.right) with
  | (Leaf, Leaf) -> if (hash value) = root.value then [{value=value; steps=[]}] else []
  | (Node l, Leaf) ->
    let subproofs = create_proofs hash value l in
    add_proof_step subproofs (Right l.value)
  | (Leaf, Node r) -> 
    let subproofs = create_proofs hash value r in
    add_proof_step subproofs (Left r.value)
  | (Node l, Node r) -> 
    let subproofs_left = create_proofs hash value l in
    let subproofs_right = create_proofs hash value r in
    List.append
      (add_proof_step subproofs_left (Right r.value))
      (add_proof_step subproofs_right (Left l.value))
