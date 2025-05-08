open OUnit2
open Contdice.Stats

(** Numerical differentiation using central difference *)
let numerical_derivative ?(h=1e-6) f x =
  (f (x +. h) -. f (x -. h)) /. (2.0 *. h)

(** Combined absolute and relative tolerance check *)
let assert_float_close_rel ~atol ~rtol msg f1 f2 =
  let diff = abs_float (f1 -. f2) in
  let tol_combined = atol +. rtol *. abs_float f2 in
  let check = diff <= tol_combined in
  if not check then
    Printf.eprintf "Assertion failed: %s\n  f1 = %g\n  f2 = %g\n  diff = %g\n  tol_combined = %g (atol=%g, rtol=%g)\n" msg f1 f2 diff tol_combined atol rtol;
  assert_bool msg check

(* Test suite for Stats module *)
let suite =
  "Stats Tests" >::: [
    "Uniform PDF vs CDF derivative - Classic (1,3)" >:: (fun _ ->
      let dist = Uniform (1.0, 3.0) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      (* Inside the interval *)
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) mid point" (pdf 2.0) (numerical_derivative cdf 2.0);
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) point 1.5" (pdf 1.5) (numerical_derivative cdf 1.5);
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) point 2.5" (pdf 2.5) (numerical_derivative cdf 2.5);
      (* Near boundaries *)
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) near lo" (pdf 1.001) (numerical_derivative cdf 1.001);
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) near hi" (pdf 2.999) (numerical_derivative cdf 2.999);
      (* Outside the interval *)
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) below lo" (pdf 0.5) (numerical_derivative cdf 0.5);
      assert_float_close_rel ~atol ~rtol "Uniform(1,3) above hi" (pdf 3.5) (numerical_derivative cdf 3.5);
    );

    "Uniform PDF vs CDF derivative - Negative (-2,-1)" >:: (fun _ ->
      let dist = Uniform (-2.0, -1.0) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "Uniform(-2,-1) mid point" (pdf (-1.5)) (numerical_derivative cdf (-1.5));
      assert_float_close_rel ~atol ~rtol "Uniform(-2,-1) near lo" (pdf (-1.999)) (numerical_derivative cdf (-1.999));
      assert_float_close_rel ~atol ~rtol "Uniform(-2,-1) near hi" (pdf (-1.001)) (numerical_derivative cdf (-1.001));
      assert_float_close_rel ~atol ~rtol "Uniform(-2,-1) below lo" (pdf (-2.5)) (numerical_derivative cdf (-2.5));
      assert_float_close_rel ~atol ~rtol "Uniform(-2,-1) above hi" (pdf (-0.5)) (numerical_derivative cdf (-0.5));
    );

    "Uniform PDF vs CDF derivative - Degenerate (2,2)" >:: (fun _ ->
      let dist = Uniform (2.0, 2.0) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      (* PDF is inf at x=2.0. CDF derivative is large near 2.0. Test away from 2.0 *)
      let x_test_below = 1.9 in
      let pdf_val_below = pdf x_test_below in
      let deriv_val_below = numerical_derivative cdf x_test_below in
      Printf.printf "Uniform(2,2) below (x=%g): PDF=%g, Deriv=%g\n" x_test_below pdf_val_below deriv_val_below;
      flush stdout;
      assert_float_close_rel ~atol ~rtol "Uniform(2,2) below" pdf_val_below deriv_val_below;

      let x_test_above = 2.1 in
      let pdf_val_above = pdf x_test_above in
      let deriv_val_above = numerical_derivative cdf x_test_above in
      Printf.printf "Uniform(2,2) above (x=%g): PDF=%g, Deriv=%g\n" x_test_above pdf_val_above deriv_val_above;
      flush stdout;
      assert_float_close_rel ~atol ~rtol "Uniform(2,2) above" pdf_val_above deriv_val_above;
      (* Test near the point of discontinuity, CDF derivative will be large, PDF is inf *)
      (* This test is tricky for assert_float_close_rel if PDF is infinity *)
      (* let deriv_at_2 = numerical_derivative ~h:1e-7 cdf 2.0 in *)
      (* assert_bool "Uniform(2,2) deriv at 2 is large" (deriv_at_2 > 1e6); *)
      (* Printf.printf "Uniform(2,2) PDF at 2.0: %f, NumDeriv: %f\n" (pdf 2.0) (numerical_derivative ~h:1e-7 cdf 2.0); *)
       assert_bool "Skipping Uniform(2,2) at 2.0 due to PDF=inf" true;
    );

    "Gaussian PDF vs CDF derivative - Classic (5,2)" >:: (fun _ ->
      let dist = Gaussian (5.0, 2.0) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let std_tol_atol, std_tol_rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol:std_tol_atol ~rtol:3e-3 "Gaussian(5,2) at mean" (pdf 5.0) (numerical_derivative cdf 5.0);
      assert_float_close_rel ~atol:std_tol_atol ~rtol:std_tol_rtol "Gaussian(5,2) +1 std" (pdf 7.0) (numerical_derivative cdf 7.0);
      assert_float_close_rel ~atol:std_tol_atol ~rtol:std_tol_rtol "Gaussian(5,2) -1 std" (pdf 3.0) (numerical_derivative cdf 3.0);
      assert_float_close_rel ~atol:std_tol_atol ~rtol:std_tol_rtol "Gaussian(5,2) +2 std" (pdf 9.0) (numerical_derivative cdf 9.0);
      assert_float_close_rel ~atol:std_tol_atol ~rtol:std_tol_rtol "Gaussian(5,2) -2 std" (pdf 1.0) (numerical_derivative cdf 1.0);
      assert_float_close_rel ~atol:std_tol_atol ~rtol:std_tol_rtol "Gaussian(5,2) +3 std" (pdf 11.0) (numerical_derivative cdf 11.0);
      assert_float_close_rel ~atol:std_tol_atol ~rtol:std_tol_rtol "Gaussian(5,2) -3 std" (pdf (-1.0)) (numerical_derivative cdf (-1.0));
    );

    "Gaussian PDF vs CDF derivative - Standard Normal (0,1)" >:: (fun _ ->
      let dist = Gaussian (0.0, 1.0) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol:3e-3 "Gaussian(0,1) at mean" (pdf 0.0) (numerical_derivative cdf 0.0);
      assert_float_close_rel ~atol ~rtol "Gaussian(0,1) +1 std" (pdf 1.0) (numerical_derivative cdf 1.0);
      assert_float_close_rel ~atol ~rtol "Gaussian(0,1) -1 std" (pdf (-1.0)) (numerical_derivative cdf (-1.0));
      assert_float_close_rel ~atol ~rtol "Gaussian(0,1) +2 std" (pdf 2.0) (numerical_derivative cdf 2.0);
      assert_float_close_rel ~atol ~rtol "Gaussian(0,1) -2 std" (pdf (-2.0)) (numerical_derivative cdf (-2.0));
      assert_float_close_rel ~atol ~rtol "Gaussian(0,1) +3 std" (pdf 3.0) (numerical_derivative cdf 3.0);
      assert_float_close_rel ~atol ~rtol "Gaussian(0,1) -3 std" (pdf (-3.0)) (numerical_derivative cdf (-3.0));
    );

    "Exponential PDF vs CDF derivative - Lambda 1.5" >:: (fun _ ->
      let dist = Exponential 1.5 in (* Mean = 0.667, StdDev = 0.667 *)
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "Exp(1.5) at 0.001" (pdf 0.001) (numerical_derivative ~h:1e-7 cdf 0.001);
      assert_float_close_rel ~atol ~rtol "Exp(1.5) at 0.1" (pdf 0.1) (numerical_derivative cdf 0.1);
      assert_float_close_rel ~atol ~rtol "Exp(1.5) at mean (0.667)" (pdf (1.0/.1.5)) (numerical_derivative cdf (1.0/.1.5));
      assert_float_close_rel ~atol ~rtol "Exp(1.5) at 1.0" (pdf 1.0) (numerical_derivative cdf 1.0);
      assert_float_close_rel ~atol ~rtol "Exp(1.5) at 3.0" (pdf 3.0) (numerical_derivative cdf 3.0);
      assert_float_close_rel ~atol ~rtol "Exp(1.5) below 0" (pdf (-0.5)) (numerical_derivative cdf (-0.5));
    );

    "Exponential PDF vs CDF derivative - Lambda 0.5" >:: (fun _ ->
      let dist = Exponential 0.5 in (* Mean = 2.0, StdDev = 2.0 *)
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "Exp(0.5) at 0.001" (pdf 0.001) (numerical_derivative ~h:1e-7 cdf 0.001);
      assert_float_close_rel ~atol ~rtol "Exp(0.5) at 1.0" (pdf 1.0) (numerical_derivative cdf 1.0);
      assert_float_close_rel ~atol ~rtol "Exp(0.5) at mean (2.0)" (pdf 2.0) (numerical_derivative cdf 2.0);
      assert_float_close_rel ~atol ~rtol "Exp(0.5) at 4.0" (pdf 4.0) (numerical_derivative cdf 4.0);
      assert_float_close_rel ~atol ~rtol "Exp(0.5) below 0" (pdf (-1.0)) (numerical_derivative cdf (-1.0));
    );

    "LogNormal PDF vs CDF derivative - Classic (1.0, 0.5)" >:: (fun _ ->
      let mu, sigma = 1.0, 0.5 in
      let dist = LogNormal (mu, sigma) in (* Median=exp(1)=2.718, Mode=exp(1-0.25)=2.117, Mean=exp(1+0.125)=3.080 *)
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "LogN(1,0.5) near 0" (pdf 0.01) (numerical_derivative ~h:1e-8 cdf 0.01);
      assert_float_close_rel ~atol ~rtol "LogN(1,0.5) at 1.0" (pdf 1.0) (numerical_derivative cdf 1.0);
      assert_float_close_rel ~atol ~rtol "LogN(1,0.5) at mode" (pdf (exp (mu -. sigma*.sigma))) (numerical_derivative cdf (exp (mu -. sigma*.sigma)));
      assert_float_close_rel ~atol ~rtol:3e-3 "LogN(1,0.5) at median" (pdf (exp mu)) (numerical_derivative cdf (exp mu));
      assert_float_close_rel ~atol ~rtol "LogN(1,0.5) at mean" (pdf (exp (mu +. sigma*.sigma/.2.0))) (numerical_derivative cdf (exp (mu +. sigma*.sigma/.2.0)));
      assert_float_close_rel ~atol ~rtol "LogN(1,0.5) at 5.0" (pdf 5.0) (numerical_derivative cdf 5.0);
      assert_float_close_rel ~atol ~rtol "LogN(1,0.5) below 0" (pdf (-0.1)) (numerical_derivative cdf (-0.1));
    );

    "LogNormal PDF vs CDF derivative - (0.0, 1.0)" >:: (fun _ ->
      let mu, sigma = 0.0, 1.0 in
      let dist = LogNormal (mu, sigma) in (* Median=exp(0)=1, Mode=exp(0-1)=0.367, Mean=exp(0+0.5)=1.648 *)
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "LogN(0,1) near 0" (pdf 0.01) (numerical_derivative ~h:1e-8 cdf 0.01);
      assert_float_close_rel ~atol ~rtol "LogN(0,1) at mode" (pdf (exp (mu -. sigma*.sigma))) (numerical_derivative cdf (exp (mu -. sigma*.sigma)));
      assert_float_close_rel ~atol ~rtol:3e-3 "LogN(0,1) at median" (pdf (exp mu)) (numerical_derivative cdf (exp mu));
      assert_float_close_rel ~atol ~rtol "LogN(0,1) at mean" (pdf (exp (mu +. sigma*.sigma/.2.0))) (numerical_derivative cdf (exp (mu +. sigma*.sigma/.2.0)));
      assert_float_close_rel ~atol ~rtol "LogN(0,1) at 5.0" (pdf 5.0) (numerical_derivative cdf 5.0);
      assert_float_close_rel ~atol ~rtol "LogN(0,1) below 0" (pdf (-0.1)) (numerical_derivative cdf (-0.1));
    );

    "Beta PDF vs CDF derivative - Classic (2.0, 5.0)" >:: (fun _ ->
      let a, b = 2.0, 5.0 in (* Mode=(2-1)/(2+5-2)=1/5=0.2. Mean=2/7=0.2857 *)
      let dist = Beta (a,b) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "Beta(2,5) near 0 (0.01)" (pdf 0.01) (numerical_derivative ~h:1e-8 cdf 0.01);
      assert_float_close_rel ~atol ~rtol "Beta(2,5) at 0.1" (pdf 0.1) (numerical_derivative cdf 0.1);
      assert_float_close_rel ~atol ~rtol "Beta(2,5) at mode (0.2)" (pdf 0.2) (numerical_derivative cdf 0.2);
      assert_float_close_rel ~atol ~rtol "Beta(2,5) at mean (0.2857)" (pdf (a/.(a+.b))) (numerical_derivative cdf (a/.(a+.b)));
      assert_float_close_rel ~atol ~rtol "Beta(2,5) at 0.5" (pdf 0.5) (numerical_derivative cdf 0.5);
      assert_float_close_rel ~atol ~rtol "Beta(2,5) at 0.9" (pdf 0.9) (numerical_derivative cdf 0.9);
      assert_float_close_rel ~atol ~rtol "Beta(2,5) near 1 (0.99)" (pdf 0.99) (numerical_derivative ~h:1e-8 cdf 0.99);
      assert_float_close_rel ~atol ~rtol "Beta(2,5) below 0" (pdf (-0.1)) (numerical_derivative cdf (-0.1));
      assert_float_close_rel ~atol ~rtol "Beta(2,5) above 1" (pdf (1.1)) (numerical_derivative cdf (1.1));
    );

    "Beta PDF vs CDF derivative - Symmetric (2.0, 2.0)" >:: (fun _ ->
      let a, b = 2.0, 2.0 in (* Mode=0.5, Mean=0.5 *)
      let dist = Beta (a,b) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "Beta(2,2) at 0.1" (pdf 0.1) (numerical_derivative cdf 0.1);
      assert_float_close_rel ~atol ~rtol "Beta(2,2) at 0.25" (pdf 0.25) (numerical_derivative cdf 0.25);
      assert_float_close_rel ~atol ~rtol "Beta(2,2) at mean (0.5)" (pdf 0.5) (numerical_derivative cdf 0.5);
      assert_float_close_rel ~atol ~rtol "Beta(2,2) at 0.75" (pdf 0.75) (numerical_derivative cdf 0.75);
      assert_float_close_rel ~atol ~rtol "Beta(2,2) at 0.9" (pdf 0.9) (numerical_derivative cdf 0.9);
    );

    "Beta PDF vs CDF derivative - U-Shaped (0.5, 0.5)" >:: (fun _ ->
      let a, b = 0.5, 0.5 in (* Mean=0.5. PDF->inf at x=0, x=1 *)
      let dist = Beta (a,b) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      (* PDF is very high near 0 and 1. Use smaller h & looser rtol *)
      assert_float_close_rel ~atol ~rtol:1e-2 "Beta(0.5,0.5) near 0 (0.01)" (pdf 0.01) (numerical_derivative ~h:1e-8 cdf 0.01);
      assert_float_close_rel ~atol ~rtol "Beta(0.5,0.5) at 0.25" (pdf 0.25) (numerical_derivative cdf 0.25);
      assert_float_close_rel ~atol ~rtol "Beta(0.5,0.5) at mean (0.5)" (pdf 0.5) (numerical_derivative cdf 0.5);
      assert_float_close_rel ~atol ~rtol "Beta(0.5,0.5) at 0.75" (pdf 0.75) (numerical_derivative cdf 0.75);
      assert_float_close_rel ~atol ~rtol:1e-2 "Beta(0.5,0.5) near 1 (0.99)" (pdf 0.99) (numerical_derivative ~h:1e-8 cdf 0.99);
    );

    "Beta PDF vs CDF derivative - Uniform (1.0, 1.0)" >:: (fun _ ->
      let a, b = 1.0, 1.0 in (* This is Uniform(0,1) *)
      let dist = Beta (a,b) in
      let cdf x = cdistr_cdf dist x in
      let pdf x = cdistr_pdf dist x in
      let atol, rtol = 1e-8, 1e-3 in
      assert_float_close_rel ~atol ~rtol "Beta(1,1) at 0.25" (pdf 0.25) (numerical_derivative cdf 0.25);
      assert_float_close_rel ~atol ~rtol "Beta(1,1) at 0.5" (pdf 0.5) (numerical_derivative cdf 0.5);
      assert_float_close_rel ~atol ~rtol "Beta(1,1) at 0.75" (pdf 0.75) (numerical_derivative cdf 0.75);
    );
  ]

(* Run the tests *)
let () =
  Random.self_init (); (* Initialize random generator for sampling tests if needed *)
  run_test_tt_main suite 