open Petrol
open Petrol.Postgres

type affiliation =
  { id   : int
  ; test : string
  }

let string_of_query q = Format.asprintf "%a" Query.pp q

let () =
  let a = { id=3; test="3" } in
  ignore (a.id, a.test);
  Out_channel.(flush stdout)

let uuid = Petrol.Type.custom ~ty:Caqti_type.string ~repr:"uuid"

let affiliation, Expr.[id; _test; _referee_id; time] =
  StaticSchema.declare_table
    ~constraints:[Schema.table_unique ["referree_id"; "referred_id"]]
    ~name:"affiliation"
    Schema.[
      field "id" ~ty:Type.int ~constraints:[primary_key ()];
      field "test" ~ty:(Type.null_ty Type.text) ~constraints:[primary_key ()];
      field "referee_id" ~ty:uuid ~constraints:[not_null ()];
      field "time" ~ty:Type.time ~constraints:[not_null ()]
    ]

let () =
  Schema.to_sql
    ~name:"affiliation"
    Schema.[
      field "id" ~ty:Type.int ~constraints:[primary_key ()];
      field "test" ~ty:Type.text ~constraints:[primary_key ()];
      field "referee_id" ~ty:uuid ~constraints:[not_null ()]
    ]
    [Schema.table_unique ["referree_id"; "referred_id"]]
  |> print_endline

  (*
let test_find_affiliation () =
  let query gt = Query.select ~from:affiliation [id]
    |> Query.where Expr.((id > i gt) && (test = s_opt None) && ((word_similarity (s "a") (s "b") = f 1.0)))
  in

  let update_query =
    Query.update ~table:affiliation
      ~set:Expr.[time := current_timestamp +! interval "1 hour"]
  in

  (Format.asprintf "%a" Query.pp (update_query))
  |> print_endline;
  
  let _request ~a db =
    query a
    |> Request.make_one
    |> Petrol.find db
  in

  Alcotest.(check string)
    "same string"
    (Format.asprintf "%a" Query.pp (query 3))
    "SELECT affiliation.id\nFROM affiliation\nWHERE affiliation.id > ?"
  *)

let test_order_by () =
  let query = Query.select ~from:affiliation [id]
    |> Query.order_by ~direction:`ASC id
    |> Query.order_by ~direction:`DESC time
  in
  
  Alcotest.(check string)
    "same"
    (string_of_query query)
    "SELECT affiliation.id\nFROM affiliation\nORDER BY affiliation.time DESC, affiliation.id ASC"

let test_jsonb_contains_string () =
  let query = Query.select ~from:affiliation [id]
    |> Query.where Expr.(jsonb_contains_string time (s "sfa"))
  in
  Alcotest.(check string)
    "same"
    (string_of_query query)
    ""

let () =
  let open Alcotest in
  run "Queries" [
      "affiliation table", [
          test_case "find_affiliation" `Quick test_order_by;
          test_case "test_jsonb_contains_string" `Quick test_jsonb_contains_string
        ];
    ]
