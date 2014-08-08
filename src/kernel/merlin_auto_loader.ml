open Std

let section = Logger.section "Merlin_auto_loader"

let find_package modname =
  let filename = modname ^ ".cmi" in
  let path = ref [] in
  let path_list = Misc.Path_list.of_string_list_ref path in
  let seen_dirs = Hashtbl.create 59 in
  let has_file packagename =
    let package = Fl_package_base.query packagename in
    let dir = package.Fl_package_base.package_dir in
    if Hashtbl.mem seen_dirs dir then None
    else begin
      Hashtbl.add seen_dirs dir ();
      try
        path := [package.Fl_package_base.package_dir];
        Logger.debug section ~title:"find_package try path" dir;
        let file = Misc.find_in_path_uncap path_list filename in
        Some file
      with
      | Not_found -> None
      | exn ->
        Logger.error section ~title:"find_package" ~exn
          ("Unexpected exception with package " ^ packagename);
        None
    end in
  let packages = Fl_package_base.list_packages () in
  match List.filter_map ~f:has_file packages with
  | [] -> raise Not_found
  | [file] -> file
  | (file1 :: file2 :: _) ->
    (* FIXME: big hack ? *)
    Location.prerr_warning Location.none
      (Warnings.Multiple_definition (modname, file1, file2));
    file1

let find_unbound_module name =
  if Clflags.auto_load () then
    find_package name
  else
    raise Not_found
