(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; ======================= PARAMETER PASSING - TASK 3 ========================

      ;; Write the expression that evaluates different for:

      ;; - Call-by-reference and Call-by-need here
      (if-true "let m = 11 in let n = 14 in ( proc (z) begin set m = 8; set n = z; -(m , n) end let a = m in -(a, n))"  14)
      
      ;; - Call-by-reference and Call-by-name here

      (if-true "let x = 5 in zero?((proc (a) begin set x = 10; -(a, x) end let b = -( x, -5) in b))"  #f)
      ;; ======================= PARAMETER PASSING - TASK 3 ========================
    )
  )
)