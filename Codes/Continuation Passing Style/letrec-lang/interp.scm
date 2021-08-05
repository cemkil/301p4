(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure representation of continuations

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))


        ;;;; TASK 4 ;;;;;;;;;
        ; Implement car expression case here
        (car-exp (exp1)
                 (value-of/k exp1 env
                             (car-cont cont)))
        ; Implement cdr expression case here
        (cdr-exp (exp1)
                 (value-of/k exp1 env
                             (cdr-cont cont)))
        ; Implement null? expression case here
        (null?-exp (exp1)
                   (value-of/k exp1 env
                               (null?-cont cont)))
        ; Implement emptylist expression case here
        (emptylist-exp ()
                       (apply-cont cont (emptylist-val)))
        ; Implement your list expression case here
        (list-exp (exp1)
                  (if (null? exp1)
                      (apply-cont cont (emptylist-val))
                      (apply-cont (list-cont exp1 (emptylist-val) env cont) 'dummy)))
        ; Implement the map expression case here        
        (map-exp (exp1 exp2)
                 (value-of/k exp1 env
                             (map1-cont exp2 env cont)))
        ;;;;;;;;;;;;;;;;;;;;;;
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
            (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))

        ;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;
        ; implement "car-cont" continuation here
        (car-cont (saved-cont)
                  (apply-cont saved-cont (expval->car val)))
        ; implement "cdr-cont" continuation here
        (cdr-cont (saved-cont)
                  (apply-cont saved-cont (expval->cdr val)))
        ; implement "null?-cont" continuation here
        (null?-cont (saved-cont)
                    (apply-cont saved-cont  (expval-null? val)))
        ; implement continuation for list-exp here.
        ; hint: you will need to call value-of/k recursively, by passing this continuation as cont to value-of/k.
        (list-cont (exps vals env saved-cont)
                    (if (null? exps)
                        (let((reversed (rev (pair-val (emptylist-val) (pair-val val vals)))))
                          (apply-cont saved-cont reversed))
                        (if (equal? val 'dummy)
                            (value-of/k (car exps) env (list-cont (cdr exps) vals env saved-cont))
                            (value-of/k (car exps) env (list-cont (cdr exps) (pair-val val vals) env saved-cont)))
                                    ))
        ; implement map-exp continuation(s) here. you will notice that one continuation will not be enough.
        (map1-cont (list env saved-cont)
                   (value-of/k list env
                               (map2-cont val saved-cont)))

        (map2-cont (val1 saved-cont)
                   (let((proc (expval->proc val1)))
                     (apply-cont (apply-procedure/k-rec-cont proc val (emptylist-val) saved-cont) 'dummy)
                     ))

        (apply-procedure/k-rec-cont (proc1 from to saved-cont)
                                    (if (expval->bool (expval-null? from))
                                        (apply-cont saved-cont (rev (pair-val (emptylist-val) (pair-val val to))))
                                        (cases proc proc1
                                          (procedure (var body saved-env)
                                                     (if (equal? val 'dummy)
                                                         (value-of/k body
                                                           (extend-env var (expval->car from) saved-env)
                                                           (apply-procedure/k-rec-cont proc1 (expval->cdr from) to saved-cont))
                                                         
                                                         (value-of/k body
                                                           (extend-env var (expval->car from) saved-env)
                                                           (apply-procedure/k-rec-cont proc1 (expval->cdr from) (pair-val val to) saved-cont)))))
                                    ))

 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        )))

  (define rev (lambda (x)
    (rev2 x  (lambda (x) x))))

  (define rev2
    (lambda (x  proc)
      (if (expval->bool(expval-null? (expval->cdr x)))
          (proc (expval->car x))              
              (rev2 (expval->cdr x) 
                (lambda (y)
                  (pair-val y (proc  (expval->car x)) ))
                  ))))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))


  
  )
  


  
