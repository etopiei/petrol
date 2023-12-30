open Ppxlib
open! Ast_builder.Default

let args () =
  Deriving.Args.(empty +> arg "name" (estring __))

  (*
let record ~private_ ~record_name ~loc ~selection (labdecs : label_declaration list)
  : structure
  =
  let getter_and_setters, fields = gen_fields ~private_ ~loc labdecs in
  let create = creation_fun ~loc record_name labdecs in
  let simple_create = simple_creation_fun ~loc record_name labdecs in
  let names = List.map (Inspect.field_names labdecs) ~f:(estring ~loc) in
  let fields_module =
    if String.equal record_name "t" then "Fields" else "Fields_of_" ^ record_name
  in
  let iter = iter_fun ~loc labdecs in
  let fold = fold_fun ~loc labdecs in
  let fold_right = fold_right_fun ~loc labdecs in
  let map = map_fun ~loc labdecs in
  let map_poly = map_poly ~loc labdecs in
  let andf = and_fun ~loc labdecs in
  let orf = or_fun ~loc labdecs in
  let to_list = to_list_fun ~loc labdecs in
  let direct_iter = direct_iter_fun ~loc labdecs in
  let direct_fold = direct_fold_fun ~loc labdecs in
  let direct_fold_right = direct_fold_right_fun ~loc labdecs in
  let direct_andf = direct_and_fun ~loc labdecs in
  let direct_orf = direct_or_fun ~loc labdecs in
  let direct_map = direct_map_fun ~loc labdecs in
  let direct_to_list = direct_to_list_fun ~loc labdecs in
  let set_all_mutable_fields = set_all_mutable_fields ~loc labdecs in
  List.concat
    [ getter_and_setters
    ; [ Per_field Names, A.str_item ~loc "names" (elist ~loc names) ]
    ; fields
    ; (match private_ with
       | Private -> []
       | Public ->
         [ Iterator Make_creator, create
         ; Iterator Create, simple_create
         ; Iterator Map, map
         ])
    ; [ Iterator Iter, iter
      ; Iterator Fold, fold
      ; Iterator Map_poly, map_poly
      ; Iterator For_all, andf
      ; Iterator Exists, orf
      ; Iterator To_list, to_list
      ; Iterator Fold_right, fold_right
      ; Direct_iterator Iter, direct_iter
      ; Direct_iterator Fold, direct_fold
      ; Direct_iterator For_all, direct_andf
      ; Direct_iterator Exists, direct_orf
      ; Direct_iterator To_list, direct_to_list
      ; Direct_iterator Fold_right, direct_fold_right
      ]
    ; (match private_ with
       | Private -> []
       | Public ->
         [ Direct_iterator Map, direct_map
         ; Direct_iterator Set_all_mutable_fields, set_all_mutable_fields
         ])
    ]
  |> assemble
       ~loc
       ~selection
       ~fields_module
       ~make_module:A.mod_
       ~make_error:(fun error ->
       pstr_extension ~loc (Location.Error.to_extension error) [])

let fields_of_td (td : type_declaration) ~selection : structure =
  let { ptype_name = { txt = record_name; loc }
      ; ptype_private = private_
      ; ptype_kind
      ; _
      }
    =
    td
  in
  match ptype_kind with
  | Ptype_record labdecs ->
    check_no_collision labdecs;
    record ~private_ ~record_name ~loc ~selection labdecs
  | _ -> []
  *)

let extract_type_fields' = function
  | (_, [
    { ptype_kind=Ptype_record [{pld_name=n2; pld_type=_typ;_}; {pld_name=n1; _ }] ; _}
    ]) ->
      print_endline n1.txt;
      print_endline n2.txt;
  | _ -> failwith "unsupported record type"

let extract_type_fields =
  let open Ast_pattern in
  type_declaration ~kind:drop ~cstrs:drop ~manifest:drop ~private_:drop ~name:drop ~params:drop

let table_binding ~loc ~name =
  [%stri let table, fields =
    StaticSchema.declare_table
      ~constraints:[]
      ~name:[%e estring ~loc name]
      Schema.[]
  ]

let generate_impl ~ctxt (_rec_flag, tds) option1 =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let () = Ast_pattern.parse extract_type_fields loc (List.hd tds) () in

  let return s =
    [ table_binding ~loc ~name:s ]
  in
  return (Option.value ~default:"default_123" option1)

let generator () =
  Deriving.Generator.V2.make (args ()) generate_impl

let ppx_petrol_table =
  Deriving.add "ppx_petrol_table" ~str_type_decl:(generator ())
