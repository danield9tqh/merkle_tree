(* This module is not completed, this was an attempt at creating a merkle patricia trie *)

include Map

module CharMap = Map.Make(Char)

module Key = struct
  type t = CharMap.key list

  let rec compare (k1: t) (k2: t): int = 
    match (k1, k2) with
    | ([], []) -> 0
    | ([], _::_) -> -1
    | (_::_, []) -> 1
    | (x::xs, y::ys) ->
      let current = Char.compare x y in
      if current == 0 then compare xs ys else current
end

type key = Key.t

module KeyMap = Map.Make(Key)

type 'a gen_tree = { node: 'a; children: 'a gen_tree CharMap.t }
type 'a tree = 'a option gen_tree

type 'a hashed_node = {
  hash: string;
  node: 'a option;
}
type 'a merkle_tree = ('a hashed_node) gen_tree


let rec find (key: key) (root: 'node gen_tree): 'node option =
  match key with
  | [] -> Option.Some root.node
  | k::ks ->
    let found = (CharMap.find_opt k root.children) in
    match found with
      | None -> Option.None
      | Some child -> find ks child

(* Takes a key and builds a tree with a path to the final node
    e.g. key=abc, value="my_val"  returns the following tree
    a -> b -> c -> "my_val"
*)
let rec build_from_key (empty_node: 'node) (key: key) (value: 'node): 'node gen_tree =
  match key with
  | [] -> {node= value; children= CharMap.empty }
  | k::ks ->
    let rest = (build_from_key empty_node ks value) in
    { node= empty_node; children= (CharMap.add k rest CharMap.empty) }

let rec insert (empty_node: 'node) (key: key) (new_node: 'node) (root: 'node gen_tree): 'node gen_tree option =
  match key with
    | [] -> if root.node==empty_node then
        Option.Some { node= new_node; children= root.children; }
      else
        Option.None
    | k::ks ->
      let found = (CharMap.find_opt k root.children) in
      match found with
      | None -> Option.Some { node= root.node; children= (CharMap.add k (build_from_key empty_node ks new_node) root.children) }
      | Some child -> insert empty_node ks new_node child

let create_tree (empty_node: 'node) (to_add: (key * 'node) list): 'node gen_tree = 
  let fold_insert tree (key, value) =
    let inserted = insert empty_node key value tree in
    match inserted with
    | None -> tree   (*TODO: Just ignoring duplicate keys for now *)
    | Some new_tree -> new_tree in
  List.fold_left fold_insert { node= empty_node; children= CharMap.empty } to_add

let aggregate_hashes (_: char) (child: 'a merkle_tree) (accum: string): string = (child.node.hash ^ accum)

let rec merkleize (hash: string -> string) (root: 'a tree ): 'a merkle_tree =
  let merkleized_children = CharMap.map (merkleize hash) (root.children) in
  let combined_child_hashes = CharMap.fold aggregate_hashes merkleized_children ""  in
  {
    node= { hash= combined_child_hashes; node=root.node };
    children= merkleized_children;
  }