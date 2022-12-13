#lang racket

(require (except-in lang/htdp-intermediate-lambda lambda require define-struct))
(require (only-in lang/htdp-intermediate-lambda
                  [lambda intermediate-lambda]))

 ; have to use this version of quickcheck because of restrictions in the test engine
(require (only-in deinprogramm/quickcheck/quickcheck
                  property
                  ==>
                  quickcheck-results
                  check-result?
                  arbitrary-integer
                  arbitrary-boolean
                  arbitrary-printable-ascii-string
                  arbitrary-integer-from-to
                  arbitrary-natural
                  arbitrary-list
                  arbitrary-nonempty-list
                  arbitrary-record
                  arbitrary-procedure))

(require (only-in test-engine/test-engine add-test! 
                                          add-failed-check! 
                                          failed-check
                                          property-fail))
(require (for-syntax syntax/parse
                     racket/syntax))
(require (only-in racket/syntax-srcloc syntax-srcloc))


(provide (all-from-out lang/htdp-intermediate-lambda))
(provide require
         for-all
         ==>
         check-property)
(provide (rename-out [intermediate-lambda lambda]
                     [my-define-struct define-struct]
                     [arbitrary-integer gen:Integer]
                     [arbitrary-printable-ascii-string gen:String]
                     [arbitrary-boolean gen:Boolean]
                     [arbitrary-integer-from-to gen:Integer-from-to]
                     [arbitrary-natural gen:Natural]
                     [arbitrary-list gen:ListOf]
                     [arbitrary-nonempty-list gen:NonEmptyListOf]
                     [arbitrary-record gen:RecordOf]
                     [arbitrary-procedure gen:ProcedureOf]))



(module reader syntax/module-reader
  isl-spec)

(define-syntax (my-define-struct stx)
  (syntax-parse stx
    [(_ name vals ...)
     #:with mod-name (format-id stx "~a" (gensym))
     #:with quoted-mod-name (datum->syntax stx `(quote ,(syntax->datum #'mod-name)))
     #:with Name (format-id stx "~a"
                            (string-titlecase
                             (symbol->string
                              (syntax->datum #'name))))
     #`(begin
         (module mod-name racket
           (require (only-in lang/htdp-intermediate-lambda
                             [define-struct intermediate-define-struct]))
           (provide (except-out (all-defined-out) Name))
           (intermediate-define-struct name vals ...))
         (#%require quoted-mod-name))]))


(define-syntax for-all
  (syntax-rules ()
    [(for-all ((?id ?gen) ...) ?body0 ?body1 ...)
     (property ((?id ?gen) ...) ?body0 ?body1 ...)]))

(define (do-check-property src prop)
  (add-test! (lambda ()
               (let-values ([(ntest stamps result) (quickcheck-results prop)])
                 (if (check-result? result)
                     (begin (add-failed-check! (failed-check (property-fail (syntax-srcloc src) result) (syntax-srcloc src)))
                            #f)
                     #t)))))

(define-syntax (check-property stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (raise-syntax-error #f "`check-property' must be at the top level" stx))
  (syntax-case stx ()
    ((_ prop) #'(do-check-property #`stx prop))
    (_ (raise-syntax-error #f "`check-property' expects a single operand" stx))))