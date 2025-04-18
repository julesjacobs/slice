let test_hello () =
  (* Basic test *)
  Alcotest.(check unit) "hello returns unit" () (Contdice.hello "Test")

let () =
  let open Alcotest in
  run "Contdice" [
    "hello", [
      test_case "hello function works" `Quick test_hello;
    ];
  ]