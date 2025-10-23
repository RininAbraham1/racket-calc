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
(define (parse-number str)
  (let ([result (string->number str)])
    (if result
        (list result "") 
        #f))) 
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
(define (parse-value str)
  (let ([num-result (parse-number str)]
        [history-result (parse-history-ref str)])
    (cond
      [num-result num-result]      
      [history-result history-result]   
      [else #f])))
(start-program)