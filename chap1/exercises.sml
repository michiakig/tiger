
(* exercise 1.1 purely functional binary search trees *)

type key = string

structure Tree = struct
    datatype tree = LEAF | TREE of tree * key * tree

    val empty = LEAF

    fun insert (key, LEAF) = TREE (LEAF, key, LEAF)
      | insert (key, TREE (l, k, r)) =
        if key < k then
            TREE (insert (key, l), k, r)
        else if key > k then
            TREE (l, k, insert (key, r))
        else
            TREE (l, key, r)
(* 1.1 (a) *)
    fun member (key, LEAF) = false
      | member (key, TREE (l, k, r)) =
        if key = k then
            true
        else if key < k then
            member (key, l)
        else
            member (key, r)
end

val true = Tree.member("b",Tree.insert("a",Tree.insert("b",Tree.empty)))
val true = Tree.member("a",Tree.insert("a",Tree.empty))
val false = Tree.member("c",Tree.empty)

(* 1.1 (b) extend the binary search program to support keys and bindings *)

structure TreeMap = struct
    datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

    fun insert (LEAF, key, value) = TREE (LEAF, key, value, LEAF)
      | insert (TREE (r, k, v, l), key, value) =
        if key = k then
            TREE (r, key, value, l)
        else if key < k then
            TREE (insert (l, key, value), k, v, r)
        else
            TREE (l, k, v, insert (r, key, value))

    fun lookup (LEAF, key) = NONE
      | lookup (TREE (l, k, v, r), key) =
        if key = k then
            SOME v
        else if key < k then
            lookup (l, key)
        else
            lookup (r, key)
end

val NONE = TreeMap.lookup(TreeMap.insert(TreeMap.LEAF,"a",1),"b")
val SOME 2 = TreeMap.lookup(TreeMap.insert(TreeMap.LEAF,"c",2),"c")

(*
1.1 (c) impl is of unbalanced trees:
t s p i p f b s t
a b c d e f g h i
*)
