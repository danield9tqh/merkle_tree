include Binary
include Cryptokit

let sha256_string = hash_string (Cryptokit.Hash.sha256 ());;

let rec check_hashes (tree: Binary.binary_merkle_tree): bool =
  match (tree.left, tree.right) with
  | (Leaf, Leaf) -> true
  | (Leaf, Node n) -> tree.value = (sha256_string (n.value^n.value)) && check_hashes n
  | (Node n, Leaf) -> tree.value = (sha256_string (n.value^n.value)) && check_hashes n
  | (Node n1, Node n2) -> tree.value = (sha256_string (n1.value^n2.value)) && check_hashes n1 && check_hashes n2

let rec check_sizes (tree: Binary.binary_merkle_tree): bool =
  match (tree.left, tree.right) with
  | (Leaf, Leaf) -> tree.size == 1
  | (Leaf, Node n) -> tree.size == n.size && check_sizes n
  | (Node n, Leaf) -> tree.size == n.size && check_sizes n
  | (Node n1, Node n2) -> tree.size == n1.size + n2.size && check_sizes n1 && check_sizes n2

(* Build basic merkle tree and check validity *)
let tree1 = Binary.build_merkle_tree sha256_string ("a", ["b"; "c"; "d"; "e"])

let () = print_endline ("hashes match: " ^ (Bool.to_string (check_hashes tree1)))
let () = print_endline ("sizes match: " ^ (Bool.to_string (check_sizes tree1)))
let () = print_endline ("final size: " ^ (Int.to_string tree1.size))

(* Build tree from inserts and check that it is the same as previous tree *)

let tree2 = Binary.build_merkle_tree sha256_string ("a", ["b"; "c"; "d"; "e"])

let tree3 =
  (Binary.insert sha256_string "e"
  (Binary.insert sha256_string "d"
  (Binary.insert sha256_string "c"
  (Binary.insert sha256_string "b"
  (Binary.build_merkle_tree sha256_string ("a", []))))))

let () = print_endline ("hashes match: " ^ (Bool.to_string (check_hashes tree2)))
let () = print_endline ("sizes match: " ^ (Bool.to_string (check_sizes tree2)))
let () = print_endline ("final size: " ^ (Int.to_string tree2.size))
let () = print_endline ("root hashes match: " ^ (Bool.to_string (String.equal tree2.value tree3.value)))

(* Build a proof for an element and then check that proof against the root hash *)
let tree4 = Binary.build_merkle_tree sha256_string ("a", ["b"; "c"; "d"; "e"])
let proofs_c = Binary.create_proofs sha256_string "c" tree4

let () = print_endline ("only one proof c: " ^ (Bool.to_string (Int.equal (List.length proofs_c) 1)))
let proof_checks_c = List.map (fun proof -> check_proof  sha256_string proof tree4.value) proofs_c
let () = print_endline ("proofs check correctly c: " ^ (Bool.to_string (List.equal Bool.equal proof_checks_c [true])))

let proofs_e = Binary.create_proofs sha256_string "e" tree4

let () = print_endline ("only one proof e: " ^ (Bool.to_string (Int.equal (List.length proofs_e) 1)))
let proof_checks_e = List.map (fun proof -> check_proof  sha256_string proof tree4.value) proofs_e
let () = print_endline ("proofs check correctly e: " ^ (Bool.to_string (List.equal Bool.equal proof_checks_e [true])))