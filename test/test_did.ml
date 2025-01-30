let yojson = Alcotest.testable Yojson.Basic.pretty_print Yojson.Basic.equal

let roundtrip s =
  let json = Yojson.Basic.from_string s in
  let json' =
    Did.Document.of_json_exn (Yojson.Basic.from_string s)
    |> Did.Document.to_json
  in
  Alcotest.check yojson "same json" json json'

let did_roundtrip s =
  let did = Did.of_string_exn s in
  Alcotest.(check string) "same did" s (Did.to_uri did |> Uri.to_string);
  did

let test_fragment () =
  let did = did_roundtrip "did:example:123456789abcdefghi#key-1" in
  Alcotest.(check (option string))
    "same fragment" (Some "key-1") (Did.fragment did)

let () =
  Alcotest.run "ocaml-did"
    [
      ("dids", [ Alcotest.test_case "fragments" `Quick test_fragment ]);
      ( "documents",
        [
          Alcotest.test_case "example-9" `Quick (fun () ->
              roundtrip Examples.nine);
          Alcotest.test_case "example-13" `Quick (fun () ->
              roundtrip Examples.thirteen);
        ] );
    ]
