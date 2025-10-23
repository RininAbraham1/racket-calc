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
       (displayln (string-append "Got: " user-input))
       (main-loop history)])))


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
      (let* ([num-str (substring str 1)]
             [history-id (string->number num-str)])
        (if history-id
            (list history-id (substring str (+ 1 (string-length (number->string history-id)))))
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


(define (evaluate-expression str history)
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
                   (evaluate-operator value remaining history)]
                  [else #f]))
              #f)))))

(define (evaluate-operator operator remaining-str history)
  (cond
    [(member operator '(+ *))
     (let* ([arg1-result (evaluate-expression remaining-str history)])
       (if arg1-result
           (let* ([arg1 (first arg1-result)]
                  [after-arg1 (second arg1-result)]
                  [arg2-result (evaluate-expression after-arg1 history)])
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