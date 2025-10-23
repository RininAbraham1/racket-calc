#lang racket

(define show-prompts?
  (let ([arguments (current-command-line-arguments)])
    (cond
      [(= (vector-length arguments) 0) #t]  
      [(string=? (vector-ref arguments 0) "-b") #f]  
      [(string=? (vector-ref arguments 0) "--batch") #f]  
      [else #t])))  

(define (start-program)
  (main-loop '()))  

(define (main-loop history)
  (when show-prompts?  
    (display "> "))
  
  (let ([user-input (read-line)])
    (cond
      [(eof-object? user-input) (void)]  
      [(string=? user-input "quit") (void)]  
      [else
       (let ([result (evaluate-expression user-input history)])
         (if result
             (let ([value (first result)]
                   [remaining (second result)])
               (if (string=? (string-trim remaining) "")
                   (let ([new-history (cons value history)]
                         [history-id (+ 1 (length history))])
                     (display (format "~a: " history-id))
                     (displayln (real->double-flonum value))
                     (main-loop new-history))
                   (begin
                     (displayln "Error: Invalid Expression")
                     (main-loop history))))
             (begin
               (displayln "Error: Invalid Expression")
               (main-loop history))))])))

(define (get-numeric-prefix str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) (substring str 0 i)]
      [(char-numeric? (string-ref str i)) (loop (+ i 1))]
      [(and (> i 0) (char=? (string-ref str i) #\.)) 
       (let ([next-i (+ i 1)])
         (if (and (< next-i (string-length str)) 
                  (char-numeric? (string-ref str next-i)))
             (loop next-i)
             (substring str 0 i)))]
      [else (substring str 0 i)])))

(define (parse-number-token str)
  (let ([trimmed (string-trim str)])
    (if (string=? trimmed "")
        #f
        (let ([first-char (string-ref trimmed 0)])
          (if (char-numeric? first-char)
              (let ([num-str (get-numeric-prefix trimmed)])
                (if num-str
                    (let ([num (string->number num-str)])
                      (list num (string-trim (substring trimmed (string-length num-str)))))
                    #f))
              #f)))))

(define (parse-history-ref str)
  (if (and (>= (string-length str) 2)
           (char=? (string-ref str 0) #\$)
           (char-numeric? (string-ref str 1)))
      (let* ([num-part (get-numeric-prefix (substring str 1))]
             [history-id (string->number num-part)])
        (if history-id
            (list (string->symbol (format "$~a" history-id)) 
                  (substring str (+ 1 (string-length num-part))))
            #f))
      #f))

(define (parse-operator-token str)
  (let ([trimmed (string-trim str)])
    (cond
      [(and (>= (string-length trimmed) 1) (char=? (string-ref trimmed 0) #\+)) 
       (list '+ (string-trim (substring trimmed 1)))]
      [(and (>= (string-length trimmed) 1) (char=? (string-ref trimmed 0) #\*)) 
       (list '* (string-trim (substring trimmed 1)))]
      [(and (>= (string-length trimmed) 1) (char=? (string-ref trimmed 0) #\/)) 
       (list '/ (string-trim (substring trimmed 1)))]
      [(and (>= (string-length trimmed) 1) (char=? (string-ref trimmed 0) #\-)) 
       (list '- (string-trim (substring trimmed 1)))]
      [else #f])))

(define (parse-token str)
  (let ([trimmed (string-trim str)])
    (cond
      [(parse-number-token trimmed) => (lambda (result) result)]
      [(parse-history-ref trimmed) => (lambda (result) result)]
      [(parse-operator-token trimmed) => (lambda (result) result)]
      [else #f])))

(define (parse-sub-expression str history)
  (let ([trimmed (string-trim str)])
    (if (string=? trimmed "")
        #f
        (let ([token-result (parse-token trimmed)])
          (if token-result
              (let ([value (first token-result)]
                    [remaining (second token-result)])
                (cond
                  [(number? value) (list value remaining)]
                  [(symbol? value)  
                   (if (and (>= (string-length (symbol->string value)) 2)
                            (char=? (string-ref (symbol->string value) 0) #\$))
                       (let* ([id-str (substring (symbol->string value) 1)]
                              [history-id (string->number id-str)]
                              [history-index (- history-id 1)])
                         (if (and history-id (>= history-index 0) (< history-index (length history)))
                             (list (list-ref (reverse history) history-index) remaining)
                             #f))
                       (evaluate-operator value remaining history))]
                  [else #f]))
              #f)))))

(define (evaluate-expression str history)
  (let ([result (parse-sub-expression str history)])
    (if result
        (let ([value (first result)]
              [remaining (second result)])
          (if (string=? (string-trim remaining) "")
              (list value "")
              #f))
        #f)))

(define (evaluate-operator operator remaining-str history)
  (cond
    [(member operator '(+ *))
     (let* ([arg1-result (parse-sub-expression remaining-str history)])
       (if arg1-result
           (let* ([arg1 (first arg1-result)]
                  [after-arg1 (second arg1-result)]
                  [arg2-result (parse-sub-expression after-arg1 history)])
             (if arg2-result
                 (let* ([arg2 (first arg2-result)]
                        [after-arg2 (second arg2-result)])
                   (cond
                     [(eq? operator '+) (list (+ arg1 arg2) after-arg2)]
                     [(eq? operator '*) (list (* arg1 arg2) after-arg2)]))
                 #f))
           #f))]
    [else #f]))

(start-program)