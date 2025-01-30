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
  Alcotest.(check string) "same did" s (Did.to_uri did |> Uri.to_string)

let test_fragment () = did_roundtrip "did:example:123456789abcdefghi#key-1"

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
