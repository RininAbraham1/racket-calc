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


(start-program)