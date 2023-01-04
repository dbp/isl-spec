#lang racket

(require (except-in lang/htdp-intermediate-lambda lambda require define-struct))
(require (only-in lang/htdp-intermediate-lambda
                  [lambda intermediate-lambda]
                  [require intermediate-require]))

 ; have to use this version of quickcheck result because of restrictions in the test engine
(require (only-in deinprogramm/quickcheck/quickcheck
                  make-result))

(require (except-in quickcheck make-result))

(require (only-in test-engine/test-engine add-test! 
                                          add-failed-check! 
                                          failed-check
                                          property-fail
                                          property-error))
(require test-engine/srcloc)

(require (for-syntax syntax/parse
                     racket/syntax))
(require (for-syntax (only-in racket/syntax-srcloc syntax-srcloc)
                     (only-in racket/string string-replace)))

(provide (all-from-out lang/htdp-intermediate-lambda))
(provide for-all
         ==>
         check-property
         bind-generators
         )
(provide (rename-out [intermediate-lambda lambda]
                     [intermediate-require require]
                     [my-define-struct define-struct]
                     [arbitrary-integer Integer]
                     [arbitrary-printable-ascii-string String]
                     [arbitrary-boolean Boolean]
                     ;[arbitrary-integer-from-to Integer-from-to]
                     [arbitrary-natural Natural]
                     [arbitrary-list ListOf]
                     ;[arbitrary-nonempty-list NonEmptyListOf]
                     [arbitrary-record RecordOf]
                     [arbitrary-procedure ProcedureOf]
                     [arbitrary-procedure ->]
                     [choose-integer cInteger]
                     [choose-one-of cOneOf]))



(module reader syntax/module-reader
  isl-spec)


(define-for-syntax (struct-name->signature-name struct-name)
  (string-replace (string-titlecase (symbol->string struct-name))
                  "-" ""))

(define-syntax (my-define-struct stx)
  (syntax-parse stx
    [(_ name vals ...)
     #:with mod-name (format-id stx "~a" (gensym))
     #:with quoted-mod-name (datum->syntax stx `(submod "." ,(syntax->datum #'mod-name)))
     #:with Name (format-id stx "~a"
                            (struct-name->signature-name
                             (syntax->datum #'name)))
     (syntax-local-lift-module
     #'(module mod-name racket
         (require (only-in lang/htdp-intermediate-lambda
                           [define-struct dstruct]))
         (provide (except-out (all-defined-out) Name))
         (dstruct name vals ...)))
     #'(begin (#%require quoted-mod-name))]))


(define-syntax for-all
  (syntax-rules ()
    [(for-all ((?id ?gen) ...) ?body0 ?body1 ...)
     (property ((?id ?gen) ...) ?body0 ?body1 ...)]))


;; Much of this is modified from a version within deinprogramm/quickcheck or htdp-lib, since they aren't exported
(define (do-check-property srcloc prop)
  (add-test!
   (lambda ()
     (with-handlers ((exn:fail?
                      (lambda (e)
                        (add-failed-check! (failed-check (property-error srcloc e)
                                                         (exn-srcloc e))))))
       (call-with-values
        (lambda ()
          #;(with-handlers
              ((exn:assertion-violation?
                (lambda (e)
                  ;; minor kludge to produce comprehensible error message
                  (if (eq? (exn:assertion-violation-who e) 'coerce->result-generator)
                      (raise (make-exn:fail (string-append "Value must be property or boolean: "
                                                           ((error-value->string-handler)
                                                            (car (exn:assertion-violation-irritants e))
                                                            100))
                                            (exn-continuation-marks e)))
                      (raise e))))))
            (quickcheck-results prop))
        (lambda (ntest stamps result)
          (if (result? result)
              (begin (add-failed-check!
                      (failed-check (property-fail srcloc (make-result (result-ok result)
                                                                       (result-stamp result)
                                                                       (result-arguments-list result))) srcloc))
                     #f)
              #t)))))))
       
  #;(add-test!
   (lambda ()
     (let-values ([(ntest stamps result) (quickcheck-results prop)])
       (if (check-result? result)
           (begin (add-failed-check!
                   (failed-check (property-fail srcloc result) srcloc))
                  #f)
           #t))))

(define-syntax (check-property stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (raise-syntax-error #f "`check-property' must be at the top level" stx))
  (syntax-case stx ()
    ((_ prop) #`(do-check-property #,(syntax-srcloc stx) prop))
    (_ (raise-syntax-error #f "`check-property' expects a single operand" stx))))