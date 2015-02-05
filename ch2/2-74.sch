;a

(define (get-record employee-id personnel-file)
  ((get 'retrive-employee (department personnel-file)) employee-id))
;personnel files must contain a type identifying the department so that the correct retrive-employee function can be dispatched
;as long as this type is in the same place, (department) should work. The rest of the structure is left up to the department

(define (salary employee)
  ((get 'salary (department employee)) employee))
;the employee record must contain a type tag, but otherwise it's implementation is undefined

;c
(define (find-employee-record name . personnel-files)
  (car (apply (lambda (personnel-file)
	   ((get 'find-employee (deparment personnel-file)) name))
	 personnel-files)))

;d
;new departments must register find-employee, salary and retrive-employee procedures with an install package and must have a deparment on the personnel file and the employee
