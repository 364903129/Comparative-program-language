#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.5 2019-01-04 17:04:42-08 - - $
;; Partner: Haofan Wang   (hwang108@ucsc.edu)
;; Partner: Michael Zhang (mzhang62@ucsc.edu)
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;;---READ IN FILES (provided in the sample sbi.scm)--------------------

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (dump-stdin))

;;---SYMBOL TABLES----------------------------------------------------

;;label table
(define *label-table* (make-hash))  
(define (get-label key)
        (hash-ref *label-table* key))
(define (add-label! key value)
        (hash-set! *label-table* key value))

;;function table
(define *function-table* (make-hash))
(define (get-function key)
        (hash-ref *function-table* key))
(define (add-function! key value)
        (hash-set! *function-table* key value))

;;variable table
(define *variable-table* (make-hash))
(define (get-variable key)
        (hash-ref *variable-table* key))
(define (add-variable! key value)
        (hash-set! *variable-table* key value))

;;array table
(define *array-table* (make-hash))
(define (get-array key)
        (hash-ref *array-table* key))
(define (add-array! key value)
        (hash-set! *array-table* key value))

;;---initializing the function table (symbols.scm)---------------------

(for-each
    (lambda (pair)
            (add-function! (car pair) (cadr pair)))
    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        ;(e       2.718281828459045235360287471352662497757247093)
        ;(pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (/       ,/)
        (=       ,=)
        (<=      ,<=)
        (>=      ,>=)
        (<       ,<)
        (>       ,>)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (abs     ,abs)
        (sin     ,sin)
        (cos     ,cos) 
        (tan     ,tan) 
        (atan    ,(lambda (x) (atan x)))  
        (asin    ,asin) 
        (acos    ,acos) 
        (round   ,round)
        (<>      ,(lambda (x y) (not (= x y))))
     )
)

;;---initialize the variable table-------------------------------------

(for-each
    (lambda (pair)
            (add-variable! (car pair) (cadr pair)))
    `(
        (eof     0.0)
        (nan     ,(/ 0.0 0.0))
        (e       ,(exp 1.0))
        (pi      ,(acos -1.0))
     )
)

;;---GET THE LABELS FROM SBIR FILE (top-level list)--------------------
  
(define (put-labels file)
        (define current-line (car file))
        ;; if there are more than just line number on this line
        (when (> (length current-line) 1)       
        ;; -->then the following word could be a possible label
              (define possible-label (cadr current-line))
              ;; -->if it is a  label       
              (when (symbol? possible-label) 
              ;; -->add the label to the label-table                   
                    (add-label! possible-label file)))         
        ;; if there are more lines to read in the file           
        (when (> (length file) 1)   
        ;; -->call 'put labels' recursively to continue reading      
              (put-labels (cdr file))                           
              ;;(printf "***Proceeding \n")
        )
)                                                              

;;---EVALUATE EXPRESSION----------------------------------------------

(define (evaluate-expression expression)
  ;(printf "=================================\n")
  ;(printf "Evaluating expression: ~s\n" expression)
        ;; if the expression is simply a string/number, just return it
  (cond ((string? expression)  
           ;(printf "~s is a string\n" expression) 
           expression )     
        ((number? expression)  
           ;(printf "~s is a number\n" expression) 
           (+ 0.0 expression))
        ((pair? expression) 
  ;; if it is a pair, we need to further determine its identity
              ;;(define current-expression (car expression))
           ;(printf "~s is a pair\n" expression)
      (cond ((hash-has-key? *function-table* (car expression))   
          ;; check if it's a function in the function-table
              ;(printf "~s found in function-table\n" (car expression))
        (cond ((vector? (get-function (car expression)))         
            ;; need to separate this operation into two parts
              ;(printf "it is a vector in function\n")  
               ;; check both in fuction and array table
              (define vector (get-function (car expression)))
              ;(printf "~s a vector\n" (car expression)) 
              ;;(printf "=================================\n")
              (define index (inexact->exact 
                (get-function (cadr expression))))
              (vector-ref vector index))
        (else (apply (get-function (car expression)) 
            (map evaluate-expression (cdr expression))))) 
            )
      (else (hash-has-key? *array-table* (cadr expression)) 
          ;(printf "~s is found in the array-table\n" (cadr expression))
          (define vector (get-array (cadr expression)))
          (define index (inexact->exact 
            (evaluate-expression (caddr expression))))
          ;(printf "Index is: ~s\n" index)
          (define array-size (vector-length vector))
          (if (< index array-size)
            (vector-ref vector index) 0.0
          )
          ; (when (>= index array-size)
          ;   (printf "Error. Array index out of bound.\n")
          ;   (get-variable 'nan) 
          ; )
      )
    )
        ;;if the "car expression" is not found in the function table 
        ;;check if it's in the array table
              
        ;; if the expression is a pair?
        ;; -->if its in the f-table,apply(current, evaluate(remaining))
        ;; -->check if its in the variable table --> apply
        ;; -->check if its in the array table
     ) 
        ;;when we get the variable from the variable-table
        ((hash-has-key? *variable-table* expression) 
              ;(printf "~s is a variable\n" expression) 
              (get-variable expression))   
        (else expression)
        )
)

;;---INTERPRET-statement -----------------------------------------------

;;---"print" function--------------------------------------------------

;;helper-function for print-interpretation
(define (helper-print subline) 
        (define expression (car subline))
        ;(printf "Evalauting expression: ~s\n" expression)
        ;;evaluate the printable expression, continue if more
        (display " ")
        (display (evaluate-expression expression))        
        (when (> (length subline) 1)    
              (display " ")                               
              (helper-print (cdr subline)))
)


(define (print-interpretation statement program)
        ;; when subline exist, not only keyword "print" 
        (when (not (= (length statement) 1))              
                  (define subline (cdr statement))
                  ;(printf "Printing ~s\n" subline)
                  ;;use the helper function to eval more
                  (helper-print subline)        
                  (display "\n")
        )
        ;; print new line if only 'print'
        (when (= (length statement) 1)                    
              (display "\n")
        )
        ;; continue reading the file
        (when (> (length program) 1)                 
              (interpret-program (cdr program))))        

;;---"let" function---------------------------------------

(define (let-interpretation statement program)
        (define key (cadr statement))
        ;(printf "Inside let with statement: ~s\n" statement)
        ;(printf "Inside let with key: ~s\n" key)
        (define value (evaluate-expression (caddr statement)))
        ;(printf "Inside let with value: ~s\n" value)
                ;; if it's a variable, put into the v-table
        (cond ((symbol? key) (add-variable! key value) 
          ;(printf "Added Variable ~s with value ~s\n" key value)
          ) 
        ;;if it's an array, find the corresponding array reference 
        (else                                                     
           (define array-key (cadr key))
              ;(printf "Array-key is: ~s\n" array-key)
              ;(define vector (get-array (car key))) 
              (define vector (get-array array-key))              
              (define index (evaluate-expression (caddr key)))
              ;(printf "Array-index is: ~s\n" index)
              (define array-size (vector-length vector))
              (when (< index array-size)
                (vector-set! vector (inexact->exact index) value)
                ;(printf "Array ~s, index ~s, value ~s\n" 
                  ;array-key index value)
                ;(printf "Array ~s has a size of ~s\n" 
                 ; array-key (vector-length vector))
              )
              (when (>= index array-size) 
                 ;(printf "Error. Array index out of bound.\n")
                (get-variable 'nan)
              )
         ))
         ;; continue reading the file
        (when (> (length program) 1)                 
              (interpret-program (cdr program))))  

;;---"dim" function------------------------------------
(define (dim-interpretation statement program)
    ;(printf "Inside dim with statement: ~s\n" statement)
    (define variable-expression (cadr statement))
    ;(printf "Variable-expression is: ~s\n" variable-expression)
    (define array-name (cadr variable-expression))
    ;(printf "Array-name is: ~s\n" array-name)
    (define dimension 
      (evaluate-expression (caddr variable-expression)))
    ;(printf "Array-dimension is: ~s\n" dimension)
    (add-array! array-name 
      (make-vector (inexact->exact dimension)))
    ;(printf "Added array ~s with dimension ~s\n" array-name dimension)
    ;; continue reading the remaining file
    (when (> (length program) 1)                 
      (interpret-program (cdr program))))    

;;---"goto" function--------------------------------------
(define (goto-interpretation args program)
        (define label (cadr args))
        ;(printf "the label is ~s" label)
        (when not (null? (get-label label))
              (interpret-program (get-label label))))

;;---"if" function-----------------------------------------
(define (if-interpretation statement program)
        ;; statement expample (if (oper exp exp) label)
        (define expression (cadr statement))
        (define label (caddr statement))
        (cond ((evaluate-expression expression) 
          (interpret-program (get-label label)))
        ;;continue reading the file
        (else 
          (when (> (length program) 1)
              (define unread-program (cdr program))                 
              (interpret-program unread-program)))   
))

;;---"input" function-------------------------------------------
;;first version of interpret input
; (define (interpret--input statement program)
;         (printf "The statement in input is: ~s\n" statement)
;         (define expression (cdr statement))
;         (printf "The expression in input is: ~s\n" expression)
;         (define variable-name (car expression))
;         (printf "The first-input in input is: ~s\n" variable-name)
;         (cond ((symbol? variable-name) 
;                (define user-input (read))
;                (printf "The user-input is ~s\n" user-input)
;             (cond ((number? user-input) 
;                    (add-variable! variable-name user-input) 
;                 (printf "Input: ~s with value ~s\n" 
;                       variable-name user-input)
;                   )
;                ((eof-object? user-input) (add-variable! 'eof 1.0)
;                 (printf "EOF: key: ~s, value: ~s\n" 
;                   'eof (get-variable 'eof))
;                )
;             (else (add-variable! variable-name (get-variable 'nan)) 
;                   (printf "Input(non-numeric): ~s with value ~s\n" 
;                     variable-name (get-variable 'nan)))
;                )
;               )
;         )
;         ;;continue reading the file
;         (when (> (length program) 1)                 
;                     (interpret-program (cdr program)))
; )

;;---second version of interpret input
; (define (interpret---input statement program)
;   (printf "The statement in Input is: ~s\n" statement)
;   (define expression (cdr statement))
;   (printf "The expression in Input is: ~s\n" expression)
;   (define variable-name (car expression))
;   (printf "The current input in Input is: ~s\n" variable-name)
;   (define user-input (read-line))
;   (printf "The user-input from read-line is: ~s\n" 
;;(convert user-input))
;   (define list-input (convert user-input))
;   (helper-input expression list-input)
;   ;;continue reading the program
;   (when (> (length program) 1)                 
;       (interpret-program (cdr program)))
; )


;;****************CURRENT VERSION OF Input*******************
(define (input-interpretation statement program)
  ;(printf "The statement in Input is: ~s\n" statement)
  (define variables (cdr statement))
  ;(printf "The variables in Input is: ~s\n" variables)
  ;(printf "Storing variables into hashtable\n")
  ;;store variables into hashtable
  (store-input-variables variables program)
  ;;we would continue the main SBIR program after 
  ;;storing all variables
)



;;**************helper function of current input *********
(define (store-input-variables variables program)
  (when (not (null? (car variables)))  ;;when we still have variables
    (define current-variable (car variables))
    ;(printf "The current-variable in Input is: ~s\n" current-variable)
    (define user-input (readnumber))    ;;read in one from user-input
    ;;if the input is a variable, store it
    (cond ( (number? user-input)    
            (add-variable! current-variable user-input)
      ;(printf "Helper - Input: ~s with value ~s\n" 
      ;current-variable user-input)
          )
            ;;if the input is eof, change its value 
          ( (eof-object? user-input)   
            (add-variable! 'eof 1.0)
;(printf "EOF: key: ~s, value: ~s\n" 'eof (get-variable 'eof))
          )
          ;;if its not a numeric value, return 'nan
          (else (get-variable 'nan)  
                ;(printf "helper-input returned nan\n")
          )
    )
  )
  ;;check if we still have any unassigned variables
  (when (not (null? (cdr variables)))
    (define remaining-var (cdr variables))
    ;(printf "Still need to read ~s\n" remaining-var)
    (store-input-variables remaining-var program)
  )
  ;;when we finished assign all the variables, continue the program
  (when (null? (cdr variables))
     (when (> (length program) 1)                 
      (interpret-program (cdr program)))
  )
)

;;************sample code from readnum.scm ********************
  {define (readnumber)
        (let ((object (read)))
             (cond [(eof-object? object) object]
                   [(number? object) (+ object 0.0)]
                   [else (begin (printf "invalid number: ~a~n" object)
                                (readnumber))] )) }

;;--helper function for second version of input-------------

; (define (helper-input variable input)
;    (printf "Helper input: variable: ~s, input: ~s\n" variable input)
;  ;;set eof to 1 when we finished implementing all the variables
;    (when (null? variable) 
;     (printf "the variable is NULL\n") 
;     (add-variable! 'eof 1.0)      
;     (printf "EOF: key: ~s, value: ~s\n" 'eof (get-variable 'eof))
;    )
;    (when (not (null? variable)) 
;     (printf "the variable is not NULL\n")
;    (define current-variable (car variable))
;    (define current-input (car input))
;    (printf "Current variable: ~s, current input: ~s\n" 
;    current-variable current-input)
;  (printf "Is current input a number? : ~s\n" (number? current-input))
;  (printf "Is current input EOF? : ~s\n" (eof-object? current-input))
;    ;;if we get a number, store it into the v-table
;    (cond ((number? current-input) 
;            (add-variable! current-variable current-input)
;           (printf "Helper - Input: ~s with value ~s\n" 
;            current-variable current-input)
;          )
;          ;;if we encounter eof, change its value and return nan
;          ((eof-object? current-input) (add-variable! 'eof 1.0)
;     (printf "EOF: key: ~s, value: ~s\n" 'eof (get-variable 'eof))

;           ) 
;      (else (get-variable 'nan) 
;       (printf "helper-input returned nan")
;       )    
;    )
;    (when (not (null? (cdr input)))
;         (helper-input (cdr variable) (cdr input))
;    ))
; )

;;helper function that convert a string to a list of symbols?
(define (convert string)
         (map string->number 
          (string-split string)))

;;---INTERPRET-PROGRAM------------------------------------------
(define (interpret-program program)
        ;; get the current line
        (define line (car program))                          
        ;; if only linenumber, do nothing
        (when (= (length line) 1)   
              ;;continue reading the program                        
              (when (> (length program) 1)                     
                    (interpret-program (cdr program))))      

        ;; if two elements in line, evaluate the statement/label
        (when (= (length line) 2)
              (if (pair? (cadr line))  
                   ;;if it's a statement, jump to interpret-x  
                  (statement-interpretation (cadr line) program) 
                  ;;continue reading     
                  (when (> (length program) 1)               
                        (interpret-program (cdr program))))) 
                    
        ;; if line num, label, statement all in line
        (when (= (length line) 3)                             
             (when (pair? (cddr line))                        
                    (define statement (caddr line)) 
                    ;;go to the interpret-x         
                    (statement-interpretation statement program))))   

;;helper-function for interpret-program to determine the function
(define (statement-interpretation statement program)
        (define keyword (car statement))
              (cond 
                      ((eqv? keyword 'dim)
                            (dim-interpretation statement program))

                      ((eqv? keyword 'let)
                            ;(printf "***Going to LET\n")
                            (let-interpretation statement program))

                      ((eqv? keyword 'goto)
                            (goto-interpretation statement program))

                      ((eqv? keyword 'if)
                            (if-interpretation statement program))

                      ((eqv? keyword 'print)
                            ;(printf "***Going to print\n")
                            (print-interpretation statement program))

                      ((eqv? keyword 'input)
                            (input-interpretation statement program))
              )
) 
;;---MAIN FUNCTION-------------------------------------------------
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              ;(write-program-by-line sbprogfile program)
              ;;read in all the labels in the SBIR file
              (put-labels program) 
              ;;begin the actual code-interpretation task        
              (interpret-program program)                     
        )
    )
)
;(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    ;(printf "sbi.scm: interactive mode~n")
    ;)

; (when (terminal-port? *stdin*)
;       (main (vector->list (current-command-line-argument

