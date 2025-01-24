type ('y, 'ty) t = ('y, 'ty) Types.query

type join_op = Types.join_op = LEFT | RIGHT | INNER

type ('a,'c) where_fun =
  bool Expr.t -> ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a =
    ([< `SELECT_CORE | `SELECT | `DELETE | `UPDATE]) as 'a

type ('a,'b,'c) group_by_fun =
  'b Expr.expr_list -> ('c, 'a) t -> ('c, 'a) t constraint 'a = ([< `SELECT_CORE | `SELECT ] as 'a)

type ('a,'c) having_fun =
  bool Expr.t -> ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a =
    ([< `SELECT_CORE | `SELECT ]) as 'a

type ('a,'b,'d,'c) join_fun =
  ?op:Types.join_op -> on:bool Expr.t ->
  ('b, [< `SELECT_CORE | `SELECT | `TABLE ] as 'd) t
  -> ('c, 'a) t -> ('c, 'a) t
  constraint 'a = ([< `SELECT_CORE]) as 'a

type ('a,'b,'c) on_err_fun =
  ([< `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] as 'b) ->
  ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a = ([> `UPDATE | `INSERT]) as 'a

type ('a,'b,'c) on_conflict_fun =
  'b Expr.on_conflict ->
  ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a = ([> `INSERT]) as 'a

let query_values query = List.rev (Types.query_values [] query)

let pp = Types.pp_query
let show q = Format.asprintf "%a" pp q

let query_ret_ty: 'a 'b. ('a,'b) t -> 'a Type.ty_list =
  fun (type a b) (query: (a,b) t) : a Type.ty_list ->
  match query with
  | SELECT_CORE { exprs; table=_; join=_; where=_; group_by=_; having=_; as_=_ } ->
    Expr.ty_expr_list exprs
  | SELECT { core=
    SELECT_CORE { exprs; table=_; join=_; where=_; group_by=_; having=_; as_=_};
             order_by=_; limit=_; offset=_ } ->
    Expr.ty_expr_list exprs      
  | DELETE { returning; _ } -> Expr.ty_expr_list returning
  | UPDATE { returning; _ } -> Expr.ty_expr_list returning
  | INSERT { returning; _ } -> Expr.ty_expr_list returning
  | TABLE _ -> invalid_arg "TABLE is not a valid query_ret_ty"

let select exprs ~from:table_name =
  Types.SELECT_CORE {
    exprs; join=[]; table=table_name; where=None;
    group_by=None; having=None; as_=None;
  }

let select_as exprs ~from:table_name ~as_ =
  let q = Types.SELECT_CORE {
    exprs; join=[]; table=table_name; where=None;
    group_by=None; having=None; as_=Some as_;
  } in
  Types.field_expr_list ~table_name:as_ exprs, q

let update ~table:table_name ~set =
  Types.UPDATE { table=table_name; on_err=None; where=None; set; returning = [] }
let insert ~table:table_name ~values:set =
  Types.INSERT { table=table_name; on_err=None; on_conflict=None; set; returning = [] }
let delete ~from:table_name =
  Types.DELETE { table=table_name; where=None; returning = [] }

let where : ('a,'c) where_fun
  = fun  by (type a b) (table : (b, a) t) : (b, a) t ->
    let update_where where by =
      match where with
        None -> Some by
      | Some old_by -> Some Expr.Common.(by && old_by) in
    match table with
    | Types.SELECT_CORE { exprs; table; join; where; group_by; having; as_ } ->
      let where = update_where where by in
      SELECT_CORE { exprs; table; join; where; group_by; having; as_ }
    | Types.SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having; as_ }; order_by; limit; offset } ->
      let where = update_where where by in
      SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having; as_ }; order_by; limit; offset }
    | Types.DELETE ({ where ; _ } as query) ->
      let where = update_where where by in
      DELETE { query with where }
    | Types.UPDATE ({ where; _ } as query) ->
      let where = update_where where by in
      UPDATE { query with where }
    | Types.INSERT _ -> invalid_arg "where on insert clause not supported"
    | Types.TABLE _ -> invalid_arg "where on table clause not supported"

let group_by : ('a,'b,'c) group_by_fun =
  fun by (type a b) (table : (b, a) t) : (b, a) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by=_; having; as_ } ->
    SELECT_CORE { exprs; table; join; where; group_by=Some by; having; as_ }
  | Types.SELECT { core=SELECT_CORE { exprs; table; join; where; group_by=_; having; as_ }; order_by; limit; offset } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by=Some by; having; as_ }; order_by; limit; offset }
  | Types.DELETE _ 
  | Types.UPDATE _ 
  | Types.INSERT _
  | Types.TABLE _ -> invalid_arg "group by only supported on select clause"

let having : ('a,'c) having_fun =
  fun having (type a b) (table : (b, a) t) : (b, a) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having=_; as_ } ->
    SELECT_CORE { exprs; table; join; where; group_by; having=Some having; as_ }
  | Types.SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having=_; as_ }; order_by; limit; offset } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having=Some having; as_ }; order_by; limit; offset }
  | Types.DELETE _ 
  | Types.UPDATE _ 
  | Types.INSERT _
  | Types.TABLE _ -> invalid_arg "group by only supported on select clause"

let join : ('a,'b,'d,'c) join_fun =
  fun ?(op=INNER) ~on (type a b c) (ot: (b, _) t)
    (table : (c, a) t)  ->
    match table with
    | Types.SELECT_CORE { exprs; table; join; where; group_by; having; as_ } ->
      Types.SELECT_CORE {
        exprs; table;
        join=join @ [MkJoin {
          table=ot;
          on;
          join_op=op
        }];
        where; group_by; having; as_
      }
    | Types.SELECT _
    | Types.DELETE _ 
    | Types.UPDATE _ 
    | Types.INSERT _ 
    | Types.TABLE _ ->
      invalid_arg "group by only supported on select clause"

let on_err : 'a . [`ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] -> ('c, 'a) t -> ('c, 'a) t =
  fun on_err (type a) (table : (_, a) t) : (_, a) t ->
  match table with
  | Types.SELECT_CORE _
  | Types.SELECT _
  | Types.DELETE _ 
  | Types.TABLE _ -> invalid_arg "on_err only supported for update and insert"
  | Types.UPDATE query ->
    UPDATE { query with on_err = Some on_err }
  | Types.INSERT query ->
    INSERT { query with on_err = Some on_err }

let on_conflict : 'a . 'b Types.on_conflict -> ('c, 'a) t -> ('c, 'a) t =
  fun on_conflict (type a) (table : (_, a) t) : (_, a) t ->
  match table with
  | Types.SELECT_CORE _
  | Types.SELECT _
  | Types.UPDATE _ 
  | Types.DELETE _
  | Types.TABLE _ -> invalid_arg "on_conflict only supported for insert"
  | Types.INSERT query ->
    INSERT { query with on_conflict = Some on_conflict }

let table (table: Types.table_name) : (_, [>`TABLE]) t =
  Types.TABLE { table }

let limit :
  'a 'i . 
  int Types.expr -> ('a, [< `SELECT | `SELECT_CORE ] as 'i) t ->
  ('a, [> `SELECT ]) t =
  fun (type a i) by (table: (a, i) t) : (a, [> `SELECT]) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having; as_ } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having; as_ }; limit=Some by; offset=None; order_by=None}
  | Types.SELECT { core; order_by; limit=_; offset } ->
    SELECT { core; order_by; limit=Some by; offset }
  | DELETE _
  | UPDATE _
  | INSERT _
  | TABLE _ -> invalid_arg "limit only supported for select"

let offset :
  'a 'i . 
  int Types.expr -> ('a, [< `SELECT | `SELECT_CORE ] as 'i) t ->
  ('a, [> `SELECT ]) t =
  fun (type a i) by (table: (a, i) t) : (a, [> `SELECT]) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having; as_ } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having; as_ }; limit=None; offset=Some by; order_by=None}
  | Types.SELECT { core; order_by; limit; offset=_ } ->
    SELECT { core; order_by; limit; offset=Some by }
  | DELETE _
  | UPDATE _
  | INSERT _
  | TABLE _ -> invalid_arg "offset only supported for select"

let order_by :
  'a 'b. ?direction:[ `ASC | `DESC ] -> ?nulls:[ `FIRST | `LAST ] ->
  'c Types.expr -> ('a, [< `SELECT | `SELECT_CORE] as 'b) t ->
  ('a, [> `SELECT ]) t =
  fun (type a b) ?(direction=`ASC) ?nulls field (table: (a, b) t) : (a, [> `SELECT]) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having; as_ } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having; as_ }; limit=None; offset=None; order_by=Some [direction, field, nulls]}
  | Types.SELECT { core; order_by=order_by_prev; limit; offset } ->
    let order_by = match order_by_prev with
    | None -> Some Types.Order.[direction, field, nulls]
    | Some xs -> Some Types.Order.((direction, field, nulls) :: xs)
    in
    SELECT { core; order_by; limit; offset }
  | DELETE _
  | UPDATE _
  | INSERT _
  | TABLE _ -> invalid_arg "order by only supported for select"

let order_by_ :
  'a 'b. 'c Types.Order.t -> ('a, [< `SELECT | `SELECT_CORE] as 'b) t ->
  ('a, [> `SELECT ]) t =
  fun (type a b) order_list (table: (a, b) t) : (a, [> `SELECT]) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having; as_ } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having; as_ }; limit=None; offset=None; order_by=Some order_list}
  | Types.SELECT { core; order_by=_; limit; offset } ->
    SELECT { core; order_by= Some order_list; limit; offset }
  | DELETE _
  | UPDATE _
  | INSERT _
  | TABLE _ -> invalid_arg "order by only supported for select"

let returning :
    _ Types.expr_list ->
      (_, [< `UPDATE | `INSERT | `DELETE] as 'b) t ->
      (_, 'b) t =
  fun (type b) returning (table : (_, b) t) : (_, b) t ->
  match table with
  | Types.DELETE query -> DELETE { query with returning }
  | UPDATE query -> UPDATE { query with returning }
  | INSERT query -> INSERT { query with returning }
  | SELECT_CORE _ | SELECT _ | TABLE _  -> invalid_arg "returning not supported for select"
