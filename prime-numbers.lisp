;;;; File containing definition of helper funcitons used to compute RSA


;;; Checks if number is prime or not.
;;; Receives one parameter which is number for which test is conducted
;;; Returns T if number is prime, otherwise returns NIL
(defun isprime (primenum) 
(defvar numcheck) 
(setf numcheck 1)
(loop for a from 1 to (floor (sqrt primenum)) do (if (not (= a 1) ) (if (eq (rem primenum a) 0) (setf numcheck 0) ) ) )
(if (eq numcheck 1) (return-from isprime t) (return-from isprime nil) ) )


;;; Checks if two passed numbers are coprimes
;;; Receives two parameters, first one being the number against which test will be conducted
;;; Second parameter is a candidate for coprime number
;;; Returns T if second number is indeed coprime, returns NIL otherwise
(defun coprime (firstnum secondnum)
(defvar numcheck)
(if (eq (gcd firstnum secondnum) 1) (setf numcheck 1) (setf numcheck 0))
(if (eq numcheck 1) (return-from coprime t) (return-from coprime nil) ) )


;;; Function that returns all prime numbers in between certain boundaries
;;; Two parameters passed to the function are numbers in between which prime numbers will be located
;;; Returns a list of all prime numbers between two numbers
(defun primes-between (firstnum secondnum)
(defvar dummynum)
(defvar primelist)
(setf primelist '(nil))
(if (< secondnum firstnum) (progn (setf dummynum secondnum) (setf secondnum firstnum) (setf firstnum dummynum)))
(loop for a from firstnum to secondnum
 do (if (isprime a) (setf primelist (cons a primelist))
))
(setf primelist (remove nil primelist))
(return-from primes-between (reverse primelist)))

;;; Divides the passed number to it's prime factors
;;; Parameter passed to the function is number to be divided
;;; Returns a list of the numbers prime factors
(defun prime-factors (num)
(defvar listnum)
(defvar primednum)
(defvar primelist)
(setf listnum '(nil))
(setf primednum num)
(setf primelist (primes-between 2 primednum))
(loop while (> primednum 1)
 do (loop for a in primelist
 do (loop while (eq (rem primednum a) 0) do (progn (setf listnum (cons a listnum)) (setf primednum (/ primednum a))))
 )
 )
 (setf listnum (remove nil listnum))
 (return-from prime-factors (reverse listnum))
)


;;; Function that returns a list of coprime numbers for a certain number
;;; Receives one parameter which is number for which the coprimes will be computed
;;; Returns a list coprimes for the passed numer
(defun coprimes-list (num)
(defvar listnum)
(setf listnum '(nil))
(loop for a from 2 to num do (if (eq (gcd num a) 1) (setf listnum (cons a listnum))))
 (setf listnum (remove nil listnum))
 (return-from coprimes-list (reverse listnum)) 
)


;;; Function that computes the modular multiplicative inverse for two passed numbers
;;; Number return should be such that when multiplied by e and then divided by totient, the result equals 1
(defun modular-multiplicative-inverse (e totient)
(defvar d)
(defvar num)
(setf d 0)
(setf num 1)
(loop while (eq d 0)
 do (progn (if (eq (rem (* num e) totient) 1) (setf d num)) (incf num))
)
(return-from modular-multiplicative-inverse d)
)


;;; Converts a string to an array of byte values for each character in string
;;; Receives one parameter which is string
;;; Returns a list of byte values
(defun string-to-bytes (string)
(defvar charlist)
(setf charlist '(nil))
(loop for c across string do 
	(setf charlist (cons (char-code c) charlist))
)
 (setf charlist (remove nil charlist))
 (return-from string-to-bytes (reverse charlist))
)


;;; Implementation of the fermat primality test
;;; For a number that is passed to the function returns whether or not it is a possible prime
;;; Prints out the test result as string
(defun fermat-primality (num)
(defvar corpimenum)
(defvar calc)
(defvar primeindicator)
(setf coprimenum (random-coprime num))
(setf calc ((expt coprimenum (- num 1))))
(setf primeindicator (rem calc num))
(if (eq primeindicator 1) (return-from fermat-primality "Given number is a possible prime number") (return-from fermat-primality "Given number is not a prime number"))
)