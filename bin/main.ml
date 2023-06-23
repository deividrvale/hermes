let usage_msg =
  "usage: hermes <file>\n\n\
  \  The input TRS should follow a specific format.\n\
  \  The following grammar describes the main tokens.\n\n\
  \  sort, fn, var := ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*\n\n\
  \  type := sort | sort -> type\n\n\
  \  term := var | fn | term term\n\n\
  \  rewrite_rule := term => term\n\n\
  \  An input file is then described as follows:\n\n\
  \  Signature: [ fn_0 : A_1 ; ... ; fn_k : A_K ]\n\
  \  Rules: [ rule_1 ; ... ; rule_n ]\n\n\
  \  So, a signature is a list of type declarations.\n\
  \  Hermes will recognize the function symbols and give types for the \
   variables automatically.\n\
  \  A TRS is a list of rules.\n\n\
  \  As an example, we consider below the TRS implementing addition over the \
   natural\n\
  \  numbers.\n\n\
  \  Signagure: [\n\
  \    zero : nat;\n\
  \    suc : nat -> nat;\n\
  \    add : nat -> nat -> nat\n\
  \  ]\n\n\
  \  Rules: [\n\
  \    add x 0 => x;\n\
  \    add x (suc y) => suc (add x y)\n\
  \  ]\n\n\
  \  See below the list of possible calls to hermes."

let version_msg =
  "The Hermes rewriting complexity static analysis tool, version 0.1.0\n\n\
  \  This version is used in the paper \"Rewriting Complexity Analysis through \
   Tuple Interpretations\",\n\
  \  by Deivid Vale and Liye Guo.\n\
  \  "

let print_version () =
  print_endline version_msg;
  exit 0

let input_files : string list ref = ref []
let output_file = ref ""
let anon_cmd filename = input_files := filename :: !input_files

let spec_list =
  [
    ( "-o",
      Arg.Set_string output_file,
      "Set the output filename for the termination proof." );
    ("-v", Arg.Unit print_version, "Print versioning information.");
  ]

let () =
  Arg.parse spec_list anon_cmd usage_msg;

  match !input_files with
  | [] ->
      print_endline "hermes: error: no input file provided.\n";
      Arg.usage spec_list usage_msg;
      exit 1
  | _ ->
      let config = Config.get_initial_config () in
      print_string "Running Hermes with Default settings.";
      print_endline "\n\nProcessing file: ";
      Lists.print_list Fun.id !input_files;
      (* For now Hermes only print the string of the file it gets. *)
      List.iter
        (fun x ->
          let input = Io.get_file_content x in
          let _ = Prove.get_data input in
          print_string input)
        !input_files
