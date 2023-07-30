let usage_msg =
  "usage: hermes <path/to/input/file>"

let version_msg =
  "The Hermes rewriting complexity static analysis tool, version 1.0.0\nThis version is used in the paper \"Rewriting Complexity Analysis through Tuple Interpretations\" by Liye Guo and Deivid Vale."

let print_version () =
  print_endline version_msg;
  exit 0

let input_files : string list ref = ref []
let anon_cmd filename = input_files := filename :: !input_files

let spec_list =
  [("-v", Arg.Unit print_version, "Print versioning information.")]

let () =
  Arg.parse spec_list anon_cmd usage_msg;

  match !input_files with
  | [] ->
      print_endline "hermes: error: no input file provided.\n";
      Arg.usage spec_list usage_msg;
      exit 1
  | _ :: [] ->
      print_string "Running Hermes with Default settings.";
      print_endline "\n\nProcessing file: ";
      Lists.print_list Fun.id !input_files;
      (* For now Hermes only print the string of the file it gets. *)
      List.iter
        (fun x ->
          let input = Io.get_file_content x in
          let data = Prove.get_data input in
          print_string input;
          let p = Strat.Manager.run_strat data in
          Prove.tuple_prove p
        ) !input_files
  | _ ->
    print_endline "hermes: error: please provide only one input file.\n";
    Arg.usage spec_list usage_msg;
    exit 1
