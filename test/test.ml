open Petrol
open Petrol.Postgres

let uuid = Petrol.Type.custom ~ty:Caqti_type.string ~repr:"uuid"

let affiliation, Expr.[id; _test; _referee_id] =
  StaticSchema.declare_table
    ~constraints:[Schema.table_unique ["referree_id"; "referred_id"]]
    ~name:"affiliation"
    Schema.[
      field "id" ~ty:Type.int ~constraints:[primary_key ()];
      field "test" ~ty:Type.text ~constraints:[primary_key ()];
      field "referee_id" ~ty:uuid ~constraints:[not_null ()]
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

let test_find_affiliation () =
  let query gt = Query.select ~from:affiliation [id]
    |> Query.where Expr.(id > i gt)
  in

  let _request ~a db =
    query a
    |> Request.make_one
    |> Petrol.find db
  in

  Alcotest.(check string)
    "same string"
    (Format.asprintf "%a" Query.pp (query 3))
    "SELECT affiliation.id\nFROM affiliation\nWHERE affiliation.id > ?"

let () =
  let open Alcotest in
  run "Queries" [
      "affiliation table", [
          test_case "find_affiliation" `Quick test_find_affiliation;
        ];
    ]
