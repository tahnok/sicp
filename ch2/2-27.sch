 (define (deep-reverse x) 
   (if (pair? x) 
       (append (deep-reverse (cdr x))  
               (list (deep-reverse (car x)))) 
       x))