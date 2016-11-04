(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Jeremie Dimino  <jeremie(_)dimino.org>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

open Browse_tree
open Browse_raw

open Extend_protocol.Reader

type raw_info =
  [ `Constructor of Types.constructor_description
  | `Modtype of Types.module_type
  | `Modtype_declaration of Ident.t * Types.modtype_declaration
  | `None
  | `String of string
  | `Type_declaration of Ident.t * Types.type_declaration
  | `Type_scheme of Types.type_expr
  | `Variant of string * Types.type_expr option
  ]

let raw_info_printer : raw_info -> _ = function
  | `Constructor c ->
    `Print (Out_type (Browse_misc.print_constructor c))
  | `Modtype mt ->
    `Print (Out_module_type (Printtyp.tree_of_modtype mt))
  | `Modtype_declaration (id, mtd) ->
    `Print (Out_sig_item
              (Printtyp.tree_of_modtype_declaration id mtd))
  | `None -> `String ""
  | `String s -> `String s
  | `Type_declaration (id, tdecl) ->
    `Print (Out_sig_item
              (Printtyp.tree_of_type_declaration id tdecl Types.Trec_first))
  | `Type_scheme te ->
    `Print (Out_type (Printtyp.tree_of_type_scheme te))
  | `Variant (label, arg) ->
    begin match arg with
      | None -> `String label
      | Some te ->
        `Concat (label ^ " of ",
                 Out_type (Printtyp.tree_of_type_scheme te))
    end

let map_entry f entry =
  let open Protocol.Compl in
  {entry with desc = f entry.desc; info = f entry.info}

(* List methods of an object.
   Code taken from [uTop](https://github.com/diml/utop
   with permission from Jeremie Dimino. *)
let lookup_env f x env =
  try Some (f x env)
  with Not_found | Env.Error _ -> None

let rec methods_of_type env ?(acc=[]) type_expr =
  let open Types in
  match type_expr.desc with
  | Tlink type_expr | Tobject (type_expr, _) | Tpoly (type_expr, _) ->
    methods_of_type env ~acc type_expr
  | Tfield (name, _, ty, rest) ->
    methods_of_type env ~acc:((name,ty) :: acc) rest
  | Tconstr (path, _, _) -> begin
      match lookup_env Env.find_type path env with
      | None | Some { type_manifest = None } -> acc
      | Some { type_manifest = Some type_expr } ->
        methods_of_type env ~acc type_expr
    end
  | _ -> acc

let classify_node = function
  | Dummy                      -> `Expression
  | Pattern                  _ -> `Pattern
  | Expression               _ -> `Expression
  | Case                     _ -> `Pattern
  | Class_expr               _ -> `Expression
  | Class_structure          _ -> `Expression
  | Class_field              _ -> `Expression
  | Class_field_kind         _ -> `Expression
  | Module_expr              _ -> `Module
  | Module_type_constraint   _ -> `Module_type
  | Structure                _ -> `Structure
  | Structure_item           _ -> `Structure
  | Module_binding           _ -> `Module
  | Value_binding            _ -> `Type
  | Module_type              _ -> `Module_type
  | Signature                _ -> `Signature
  | Signature_item           _ -> `Signature
  | Module_declaration       _ -> `Module
  | Module_type_declaration  _ -> `Module_type
  | With_constraint          _ -> `Type
  | Core_type                _ -> `Type
  | Package_type             _ -> `Module_type
  | Row_field                _ -> `Expression
  | Value_description        _ -> `Type
  | Type_declaration         _ -> `Type
  | Type_kind                _ -> `Type
  | Type_extension           _ -> `Type
  | Extension_constructor    _ -> `Type
  | Label_declaration        _ -> `Type
  | Constructor_declaration  _ -> `Type
  | Class_type               _ -> `Type
  | Class_signature          _ -> `Type
  | Class_type_field         _ -> `Type
  | Class_declaration        _ -> `Expression
  | Class_description        _ -> `Type
  | Class_type_declaration   _ -> `Type
  | Method_call              _ -> `Expression
  | Module_binding_name      _ -> `Module
  | Module_declaration_name  _ -> `Module
  | Module_type_declaration_name _ -> `Module_type
  | Open_description _ -> `Module
  | Include_declaration _ -> `Module
  | Include_description _ -> `Module

open Protocol.Compl

let make_candidate ?get_doc ~attrs ~exact name ?loc ?path ty =
  let ident = match path with
    | Some path -> Ident.create (Path.last path)
    | None -> Extension.ident
  in
  let kind, text =
    match ty with
    | `Value v ->
      (`Value, `Type_scheme v.Types.val_type)
    | `Cons c  -> (`Constructor, `Constructor c)
    | `Label label_descr ->
      let desc =
        Types.(Tarrow (Raw_compat.Parsetree.arg_label_of_str "",
                       label_descr.lbl_res, label_descr.lbl_arg, Cok))
      in
      (`Label, `Type_scheme (Btype.newgenty desc))
    | `Mod m   ->
      begin try
          if not exact then raise Exit;
          let verbosity = !Type_utils.verbosity in
          if Type_utils.mod_smallerthan (1000 * verbosity) m = None then raise Exit;
          (`Module, `Modtype m)
        with Exit -> (`Module, `None)
      end
    | `ModType m ->
      if exact then
        (`Modtype, `Modtype_declaration (ident, (*verbose_sig env*) m))
      else
        (`Modtype, `None)
    | `Typ t ->
      (`Type, `Type_declaration (ident, t))
    | `Variant (label,arg) ->
      (`Variant, `Variant (label, arg))
  in
  (* FIXME: When suggesting variants (and constructors) with parameters,
     it could be nice to check precedence and add or not parenthesis.
  let name = match ty with
    | `Variant (_, Some _) -> "(" ^ name ^ " )"
    | _ -> name
  in*)
  let desc =
    match kind with
    | `Module | `Modtype -> `None
    | _ -> text
  in
  let info = match Raw_compat.read_doc_attributes attrs, get_doc, kind with
    | Some (str, _), _, _ -> `String str
    | None, _, (`Module | `Modtype) -> text
    | None, None, _ -> `None
    | None, Some get_doc, kind ->
      match path, loc with
      | Some p, Some loc ->
        let namespace = (* FIXME: that's just terrible *)
          match kind with
          | `Value -> `Vals
          | `Type -> `Type
          | _ -> assert false
        in
        begin match get_doc (`Completion_entry (namespace, p, loc)) with
          | `Found str -> `String str
          | _ -> `None
          | exception _ -> `None
        end
      | _, _ -> `None
  in
  {name; kind; desc; info}

let item_for_global_module name = {name; kind = `Module; desc = `None; info = `None}

let fold_variant_constructors ~env ~init ~f =
  let rec aux acc t =
    let t = Ctype.repr t in
    match t.Types.desc with
    | Types.Tvariant { Types. row_fields; row_more; row_name } ->
      let acc =
        let keep_if_present acc (lbl, row_field) =
          match row_field with
          | Types.Rpresent arg when lbl <> "" -> f ("`" ^ lbl) arg acc
          | Types.Reither (_, lst, _, _) when lbl <> "" ->
            let arg =
              match lst with
              | [ well_typed ] -> Some well_typed
              | _ -> None
            in
            f ("`" ^ lbl) arg acc
          | _ -> acc
        in
        List.fold_left ~init:acc row_fields ~f:keep_if_present
      in
      let acc =
        match row_name with
        | None -> acc
        | Some (path,te) ->
          match (Env.find_type path env).Types.type_manifest with
          | None -> acc
          | Some te -> aux acc te
      in
      aux acc row_more
    | Types.Tconstr _ ->
      let t' = try Ctype.full_expand env t with _ -> t in
      if Types.TypeOps.equal t t' then
        acc
      else
        aux acc t'
    | _ -> acc
  in
  aux init

let get_candidates ?get_doc ?target_type prefix path kind ~validate env =
  let make_weighted_candidate ?(priority=0) ~attrs ~exact name ?loc ?path ty =
    (* Just like [make_candidate] but associates some metadata to the candidate.
       The candidates are later sorted using these metadata.

       The ordering works as follow:
       - first we compare the priority of the candidates
       - we compare the cost of unification for both (using Btype.total_changes)
       - if they are equal, then we compare their "binding time": things
         introduced more recently will come before older bindings (i.e. we
         prioritize the local context)
       - if these are also equal, then we just use classic string ordering on
         the candidate name. *)
    let time =
      try Ident.binding_time (Path.head (Option.get path))
      with _ -> 0
    in
    let item = make_candidate ?get_doc ~attrs ~exact name ?loc ?path ty in
    (- priority, - time, name), item
  in
  let is_internal name = name = "" || name.[0] = '_' in
  let items =
    let snap = Btype.snapshot () in
    let rec arrow_arity n t =
      match (Ctype.repr t).Types.desc with
      | Types.Tarrow (_,_,rhs,_) -> arrow_arity (n + 1) rhs
      | _ -> n
    in
    let rec nth_arrow n t =
      if n <= 0 then t else
      match (Ctype.repr t).Types.desc with
      | Types.Tarrow (_,_,rhs,_) -> nth_arrow (n - 1) rhs
      | _ -> t
    in
    let type_check =
      (* Defines the priority of a candidate.
         Priority is 1000 - cost - head_arrows, where:
         - cost is the number unification variables instantiated to make the types unify
         - head_arrows is 0 if types unified, or the number of arrows which
           have been skipped to make them unify (i.e types would unify if the
           user apply the function to head_arrows arguments).
         Note that if no type is expected (context was not inferred), 0 will be
         returned. *)
      match target_type with
      | None -> fun scheme -> 0
      | Some ty ->
        let arity = arrow_arity 0 ty in
        fun scheme ->
        let cost =
          let c = Btype.linked_variables in
          try
            let c' = c () in
            Ctype.unify_var env ty (Ctype.instance env scheme);
            c () - c'
          with _ ->
            let arity = arrow_arity (-arity) scheme in
            if arity > 0 then begin
              let c' = c () in
              Btype.backtrack snap;
              let ty' = Ctype.instance env scheme in
              let ty' = nth_arrow arity ty' in
              try Ctype.unify_var env ty ty'; arity + c () - c'
              with _ -> 1000
            end
            else 1000
        in
        Btype.backtrack snap;
        1000 - cost
    in
    let rec of_kind = function
      | `Variants ->
        begin match target_type with
        | None -> []
        | Some t ->
          fold_variant_constructors t ~init:[] ~f:(fun name param candidates ->
            if not @@ validate `Variant `Variant name then candidates else
            make_weighted_candidate name ~exact:false ~priority:2 ~attrs:[]
                (`Variant (name, param))
            :: candidates
          ) ~env
        end

      | `Values ->
        let type_check {Types. val_type} = type_check val_type in
        Env.fold_values (fun name path v candidates ->
          if not (validate `Lident `Value name) then candidates else
          let priority = if is_internal name then 0 else type_check v in
          make_weighted_candidate ~exact:(name = prefix) name ~priority ~path
            ~attrs:(Raw_compat.val_attributes v)
            (`Value v) ~loc:v.Types.val_loc
          :: candidates
        ) path env []

      | `Constructor ->
        let type_check {Types. cstr_res} = type_check cstr_res in
        Raw_compat.fold_constructors (fun name v candidates ->
          if not @@ validate `Lident `Cons name then candidates else
          let priority = if is_internal name then 0 else type_check v in
          make_weighted_candidate ~exact:(name=prefix) name (`Cons v) ~priority
            ~attrs:(Raw_compat.cstr_attributes v)
          :: candidates
        ) path env []

      | `Types ->
        Raw_compat.fold_types (fun name path decl candidates ->
          if not @@ validate `Lident `Typ name then candidates else
          make_weighted_candidate ~exact:(name = prefix) name ~path (`Typ decl)
            ~loc:decl.Types.type_loc ~attrs:(Raw_compat.type_attributes decl)
          :: candidates
        ) path env []

      | `Modules ->
        Env.fold_modules (fun name path v candidates ->
          let attrs = v.Types.md_attributes in
          let v = v.Types.md_type in
          if not @@ validate `Uident `Mod name then candidates else
            make_weighted_candidate ~exact:(name = prefix) name ~path (`Mod v) ~attrs
          :: candidates
        ) path env []

      | `Modules_type ->
        Env.fold_modtypes (fun name path v candidates ->
          if not @@ validate `Uident `Mod name then candidates else
            make_weighted_candidate ~exact:(name=prefix) name ~path (`ModType v)
              ~attrs:(Raw_compat.mtd_attributes v)
            :: candidates
        ) path env []

      | `Labels ->
        Env.fold_labels (fun ({Types.lbl_name = name} as l) candidates ->
          if not (validate `Lident `Label name) then candidates else
            make_weighted_candidate ~exact:(name = prefix) name (`Label l)
              ~attrs:(Raw_compat.lbl_attributes l)
            :: candidates
        ) path env []

      | `Group (kinds) -> List.concat_map ~f:of_kind kinds
    in
    of_kind kind
  in
  let items = List.sort items ~cmp:(fun (a,_) (b,_) -> compare a b) in
  let items = List.rev_map ~f:snd items in
  items

let gen_values = `Group [`Values; `Constructor]

let default_kinds = [`Variants; gen_values; `Types; `Modules; `Modules_type]

let completion_order = function
  | `Expression  -> [`Variants; gen_values; `Types; `Modules; `Modules_type]
  | `Structure   -> [gen_values; `Types; `Modules; `Modules_type]
  | `Pattern     -> [`Variants; `Constructor; `Modules; `Labels; `Values; `Types; `Modules_type]
  | `Module      -> [`Modules; `Modules_type; `Types; gen_values]
  | `Module_type -> [`Modules_type; `Modules; `Types; gen_values]
  | `Signature   -> [`Types; `Modules; `Modules_type; gen_values]
  | `Type        -> [`Types; `Modules; `Modules_type; gen_values]

let complete_methods ~env ~prefix obj =
  let t = obj.Typedtree.exp_type in
  let has_prefix (name,_) =
    String.is_prefixed ~by:prefix name &&
    (* Prevent identifiers introduced by type checker to leak *)
    try ignore (String.index name ' ' : int); false
    with Not_found -> true
  in
  let methods = List.filter has_prefix (methods_of_type env t) in
  List.map methods ~f:(fun (name,ty) ->
    let info = `None (* TODO: get documentation. *) in
    { name; kind = `MethodCall; desc = `Type_scheme ty; info }
  )

let complete_prefix ?get_doc ?target_type ~env ~prefix ~is_label config node =
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let find ?path ~is_label prefix =
    let valid tag name =
      try
        (* Prevent identifiers introduced by type checker to leak *)
        ignore (String.index name '-' : int);
        false
      with Not_found ->
        String.is_prefixed ~by:prefix name && uniq (tag,name)
    in
    (* Hack to prevent extensions namespace to leak
       + another to hide the "Library_name__Module" present at Jane Street *)
    let validate ident tag name =
      (if ident = `Uident
       then name <> "" && name.[0] <> '_'
            && (String.no_double_underscore name || tag <> `Mod)
       else name <> "_")
      && valid tag name
    in
    if not is_label then
      let kind = classify_node node in
      let order = completion_order kind in
      let add_completions acc kind =
        get_candidates ?get_doc ?target_type prefix path kind ~validate env @ acc
      in
      List.fold_left ~f:add_completions order ~init:[]
    else
      Env.fold_labels (fun ({Types.lbl_name = name} as l) candidates ->
        if not (valid `Label name) then candidates else
          make_candidate ?get_doc ~exact:(name = prefix) name (`Label l) ~attrs:[]
          :: candidates
      ) path env []
  in
  try
    match prefix with
    | Longident.Ldot (path, prefix) -> find ~path ~is_label prefix
    | Longident.Lident prefix ->
      let compl = find ~is_label prefix in
      (* Add modules on path but not loaded *)
      List.fold_left (Mconfig.global_modules config) ~init:compl ~f:(
        fun candidates name ->
          if not (String.no_double_underscore name) then candidates else
          let default = { name; kind = `Module; desc = `None; info = `None } in
          if name = prefix && uniq (`Mod, name) then
            try
              let path, md, attrs = Raw_compat.lookup_module (Longident.Lident name) env in
              make_candidate ?get_doc ~exact:true name ~path (`Mod md) ~attrs
              :: candidates
            with Not_found ->
              default :: candidates
          else if String.is_prefixed ~by:prefix name && uniq (`Mod,name) then
            default :: candidates
          else
            candidates
      )
    | _ -> find ~is_label (String.concat ~sep:"." @@ Longident.flatten prefix)
  with Not_found -> []

(* Propose completion from a particular node *)
let node_complete buffer ?get_doc ?target_type env node prefix =
  Printtyp.wrap_printing_env env @@ fun () ->
  match node with
  | Method_call (obj,_,_) -> complete_methods ~env ~prefix obj
  | Pattern    { Typedtree.pat_desc = Typedtree.Tpat_record (_, _) ; _ }
  | Expression { Typedtree.exp_desc = Typedtree.Texp_record (_, _) ; _ } ->
    let prefix, _is_label = Longident.(keep_suffix @@ parse prefix) in
    complete_prefix ?get_doc ?target_type ~env ~prefix ~is_label:true buffer node
  | x ->
    let prefix, is_label = Longident.(keep_suffix @@ parse prefix) in
    complete_prefix ?get_doc ?target_type ~env ~prefix ~is_label buffer node

let expand_prefix ~global_modules env prefix =
  let lidents, last =
    let ts = Expansion.explore ~global_modules env in
    Expansion.get_lidents ts prefix
  in
  let validate' =
    let last = Str.regexp (Expansion.regex_of_path_prefix last) in
    fun s -> Str.string_match last s 0
  in
  let validate _ _ s = validate' s in
  let process_lident lident =
    let candidates =
      let aux compl kind = get_candidates "" lident kind ~validate env @ compl in
      List.fold_left ~f:aux default_kinds ~init:[]
    in
    match lident with
    | None ->
      let f name =
        if not (validate' name) then None else
        Some (item_for_global_module name)
      in
      candidates @ List.filter_map global_modules ~f
    | Some lident ->
      let lident = Longident.flatten lident in
      let lident = String.concat ~sep:"." lident ^ "." in
      List.map candidates ~f:(fun c -> { c with name = lident ^ c.name })
  in
  List.concat_map ~f:process_lident lidents

open Typedtree

let application_context ~verbosity ~prefix path =
  let module Printtyp = Type_utils.Printtyp in
  let target_type = ref (
    match snd (List.Non_empty.hd path) with
    | Expression { exp_type = ty }
    | Pattern { pat_type = ty } -> Some ty
    | _ -> None
  )
  in
  let context = match path with
    | List.More ((_, Expression earg), (
        List.More ((_, Expression ({ exp_desc = Texp_apply (efun, _);
                                     exp_type = app_type; exp_env } as app)), _)
        | List.One (_, Expression ({ exp_desc = Texp_apply (efun, _);
                                      exp_type = app_type; exp_env } as app))))
      when earg != efun ->
      Printtyp.wrap_printing_env exp_env ~verbosity @@ fun () ->
      (* Type variables shared accross arguments should all be
         printed with the same name.
         [Printtyp.type_scheme] ensure that a name is unique within a given
         type, but not accross different invocations.
         [reset] followed by calls to [mark_loops] and [type_sch] provide
         that *)
      Printtyp.reset ();
      let pr t =
        let ppf, to_string = Format.to_string () in
        Printtyp.mark_loops t;
        Printtyp.type_sch ppf t;
        to_string ()
      in
      (* Special case for optional arguments applied with ~,
         get the argument wrapped inside Some _ *)
      let earg =
        match Raw_compat.optional_label_sugar earg.exp_desc with
        | None -> earg
        | Some earg ->
          target_type := Some earg.exp_type;
          earg
      in
      let labels = Raw_compat.labels_of_application ~prefix app in
      `Application { Protocol.Compl.
                     argument_type = pr earg.exp_type;
                     labels = List.map (fun (lbl,ty) -> lbl, pr ty) labels;
                   }
    | _ -> `Unknown
  in
  !target_type, context
