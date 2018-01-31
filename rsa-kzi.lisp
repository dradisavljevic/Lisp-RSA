;;;; Implementation of the few RSA based functions
;;;; Functions hadn't been proven fully safe, so their use is still not recommended

;;; RSA encryption for a passed message
;;; Function randomly selects a prime number between 1 and 10000 for encryption
;;; Encrypted message is returned
(defun rsa-blackbox-encrypt-kzi (message)
(defvar primelist)																				; Variable to hold all the prime numbers
(defvar q)																						; q parameter of the RSA algorithm
(defvar p)																						; p parameter of the RSA algorithm
(defvar n)																						; n parameter of the RSA algorithm
(defvar totient)																				; totient. Calculated as (p-1)*(q-1)
(defvar coprimelist)																			; variable that will hold all coprime numbers
(defvar e)																						; e parameter of the RSA algorithm
(defvar d)																						; d parameter of the RSA algorithm, or modular multiplicative inverse
(defvar ecmessage)																				; variable to hold an encrypted message
(defvar rsabyte)																				; variable that will hold each separate byte that will be encrypted
(setf ecmessage '(nil))																			; Setting the encrypted message initially to NIL
(setf message (string-to-bytes message))														; Converts the string to a list of byte values for each character in string
(setf primelist (primes-between 1 10000))														; Form a list of prime numbers between 1 and 10000
(setf p (nth (random (length primelist)) primelist))											; Select a random prime number from the list for p
(setf q (nth (random (length primelist)) primelist))											; Select a random prime number from the list for q
(setf n (* p q))																				; Compute n as p*q
(setf totient (lcm (- p 1) (- q 1)))															; Calculate totient
(setf coprimelist (coprimes-list totient))														; Return a list of coprime numbers for the calculated totient
(setf e (nth (random (length coprimelist)) coprimelist))										; Select one random coprime as e
(setf d (modular-multiplicative-inverse e totient))												; Calculate modular multiplicative inverse for e and totient
(loop for mbyte in message																		; For loop that goes through each byte in the list
 do (progn (setf rsabyte (rem (expt mbyte e) n)) (setf ecmessage (cons rsabyte ecmessage)))		; Encryption based on the public key (n, e) is applied to the message
)
 (setf ecmessage (remove nil ecmessage))														; Remove NIL from the message
 (setf ecmessage (reverse ecmessage))															; Since each newly encrypted character was pushed on top of the list, reverse list order
 (setf ecmessage (mapcar #'code-char ecmessage))												; Map bytes to their appropriate character values
 (setf ecmessage (coerce ecmessage 'string))													; Form a string out of the newly mapped characters
 (return-from rsa-public-kzi ecmessage) 														; Return encrypted message
)



;;; Small demo example of the RSA algorithm implemented for random prime numbers
;;; Passed message is first encrypted and the decrypted
;;; Results are printed after execution
(defun rsa-blackbox-demo-kzi (message)
(defvar primelist)																				; Variable to hold all the prime numbers
(defvar q)																						; q parameter of the RSA algorithm
(defvar p)																						; p parameter of the RSA algorithm
(defvar n)																						; n parameter of the RSA algorithm
(defvar totient)																				; totient. Calculated as (p-1)*(q-1)
(defvar coprimelist)																			; variable that will hold all coprime numbers
(defvar e)																						; e parameter of the RSA algorithm
(defvar d)																						; d parameter of the RSA algorithm, or modular multiplicative inverse
(defvar ecmessage)																				; variable to hold an encrypted message
(defvar dcmessage)																				; variable to hold a decrypted message
(defvar rsabyte)																				; variable that will hold each separate byte that will be encrypted and decrypted
(defvar messageholder)																			; swap variable to hold a message after it has been encrypted so it may be used for decryption
(setf ecmessage '(nil))																			; Setting the encrypted message initially to NIL
(setf dcmessage '(nil))																			; Setting the decrypted message initially to NIL
(setf message (string-to-bytes message))														; Converts the string to a list of byte values for each character in string
(setf primelist (primes-between 1 10000))														; Form a list of prime numbers between 1 and 10000
(setf p (nth (random (length primelist)) primelist))											; Select a random prime number from the list for p
(setf q (nth (random (length primelist)) primelist))											; Select a random prime number from the list for q
(setf n (* p q))																				; Compute n as p*q
(setf totient (lcm (- p 1) (- q 1)))															; Calculate totient
(setf coprimelist (coprimes-list totient))														; Return a list of coprime numbers for the calculated totient
(setf e (nth (random (length coprimelist)) coprimelist))										; Select one random coprime as e
(setf d (modular-multiplicative-inverse e totient))												; Calculate modular multiplicative inverse for e and totient
(loop for mbyte in message																		; For loop that goes through each byte in the list
 do (progn (setf rsabyte (rem (expt mbyte e) n)) (setf ecmessage (cons rsabyte ecmessage)))		; Encryption based on the public key (n, e) is applied to the message
)
 (setf ecmessage (remove nil ecmessage))														; Remove NIL from the message
 (setf ecmessage (reverse ecmessage))															; Since each newly encrypted character was pushed on top of the list, reverse list order
 (setf messageholder ecmessage) 																; Move encrypted message to a swap variable before converting it to string
 (setf ecmessage (mapcar #'code-char ecmessage))												; Map bytes to their appropriate character values
 (setf ecmessage (coerce ecmessage 'string))													; Form a string out of the newly mapped characters
 (print ecmessage) 																				; Print out the encrypted message
(loop for mbyte in messageholder																; Iterate through all bytes in the swap variable
 do (progn (setf rsabyte (rem (expt mbyte d) n)) (setf dcmessage (cons rsabyte dcmessage)))		; Decrypt the message with private key (n,d)
)
 (setf dcmessage (remove nil dcmessage))														; Remove NIL from the decrypted message
 (setf dcmessage (reverse dcmessage))															; Reverse the order of bytes in the list
 (setf dcmessage (mapcar #'code-char dcmessage))												; Map all bytes to their appropriate character
 (setf dcmessage (coerce dcmessage 'string))													; Form a string out of the mapped charaters
 (print dcmessage)																				; Print out the decrypted message
)

;;; Function that encypts given message through RSA with random parameters
;;; Upper boundary for the formation of the prime number list is set through the boundary parameter
;;; boundary parameter larger than 10000 can lead to slow performances
(defun rsa-blackbox-encrypt-unlimited-kzi (message boundary)
(defvar primelist)																				; Variable to hold all the prime numbers
(defvar q)																						; q parameter of the RSA algorithm
(defvar p)																						; p parameter of the RSA algorithm
(defvar n)																						; n parameter of the RSA algorithm
(defvar totient)																				; totient. Calculated as (p-1)*(q-1)
(defvar coprimelist)																			; variable that will hold all coprime numbers
(defvar e)																						; e parameter of the RSA algorithm
(defvar d)																						; d parameter of the RSA algorithm, or modular multiplicative inverse
(defvar ecmessage)																				; variable to hold an encrypted message
(defvar rsabyte)																				; variable that will hold each separate byte that will be encrypted
(setf ecmessage '(nil))																			; Setting the encrypted message initially to NIL
(setf message (string-to-bytes message))														; Converts the string to a list of byte values for each character in string
(setf primelist (primes-between 1 boundary))													; Form a list of prime numbers between 1 and the boundary parameter
(setf p (nth (random (length primelist)) primelist))											; Select a random prime number from the list for p
(setf q (nth (random (length primelist)) primelist))											; Select a random prime number from the list for q
(setf n (* p q))																				; Compute n as p*q
(setf totient (lcm (- p 1) (- q 1)))															; Calculate totient
(setf coprimelist (coprimes-list totient))														; Return a list of coprime numbers for the calculated totient
(setf e (nth (random (length coprimelist)) coprimelist))										; Select one random coprime as e
(setf d (modular-multiplicative-inverse e totient))												; Calculate modular multiplicative inverse for e and totient
(loop for mbyte in message																		; For loop that goes through each byte in the list
 do (progn (setf rsabyte (rem (expt mbyte e) n)) (setf ecmessage (cons rsabyte ecmessage)))		; Encryption based on the public key (n, e) is applied to the message
)
 (setf ecmessage (remove nil ecmessage))														; Remove NIL from the message
 (setf ecmessage (reverse ecmessage))															; Since each newly encrypted character was pushed on top of the list, reverse list order
 (setf ecmessage (mapcar #'code-char ecmessage))												; Map bytes to their appropriate character values
 (setf ecmessage (coerce ecmessage 'string))													; Form a string out of the newly mapped characters
 (return-from rsa-blackbox-encrypt-unlimited-kzi ecmessage) 									; Return encrypted message
)



;;; Function that encypts given message through RSA
;;; Values for p, q and e are set based on functions parameters
(defun rsa-encrypt-kzi (message p q e)
(if (isprime p)  																				; Condition to check if number passed for p is indeed a prime
(if (isprime q)   																				; Condition to check if number passed for q is indeed a prime. It is planned to change this in later versions to something like fermat algorith or miller-rabin, to improve perforamnce
(if (coprime (* p q) e) (progn																	; Condition to check if number passed for e is coprime of the n=(p*q)
	(defvar primelist)																			; Variable to hold all the prime numbers
	(defvar n)																					; n parameter of the RSA algorithm
	(defvar totient)																			; totient. Calculated as (p-1)*(q-1)
	(defvar coprimelist)																		; variable that will hold all coprime numbers
	(defvar d)																					; d parameter of the RSA algorithm, or modular multiplicative inverse
	(defvar ecmessage)																			; variable to hold an encrypted message
	(defvar rsabyte)																			; variable that will hold each separate byte that will be encrypted
	(setf ecmessage '(nil))																		; Setting the encrypted message initially to NIL
	(setf message (string-to-bytes message))													; Converts the string to a list of byte values for each character in string
	(setf n (* p q))																			; Compute n as p*q
	(setf totient (lcm (- p 1) (- q 1)))														; Calculate totient
	(setf d (modular-multiplicative-inverse e totient))											; Calculate modular multiplicative inverse for e and totient
	(loop for mbyte in message																	; For loop that goes through each byte in the list
	 do (progn (setf rsabyte (rem (expt mbyte e) n)) (setf ecmessage (cons rsabyte ecmessage)))	; Encryption based on the public key (n, e) is applied to the message
	)
	 (setf ecmessage (remove nil ecmessage))													; Remove NIL from the message
	 (setf ecmessage (reverse ecmessage))														; Since each newly encrypted character was pushed on top of the list, reverse list order
	 (setf ecmessage (mapcar #'code-char ecmessage))											; Map bytes to their appropriate character values
	 (setf ecmessage (coerce ecmessage 'string))												; Form a string out of the newly mapped characters
	 (return-from rsa-encrypt-kzi ecmessage)													; Return encrypted message
) (return-from rsa-encrypt-kzi "Value for e is not a coprime of totient (p*q)")					; Print out an error message if e is not coprime of n=(p*q)
) (return-from rsa-encrypt-kzi "Value for q is not a prime number")								; Print out an error message if q is not a prime number
) (return-from rsa-encrypt-kzi "Value for p is not a prime number")								; Print out an error message if p is not a prime number
))


;;; Function that decypts given message through RSA
;;; Values for p, q and e are set based on functions parameters
(defun rsa-decrypt-kzi (bytes p q e)
(if (isprime p)  																				; Condition to check if number passed for p is indeed a prime
(if (isprime q)   																				; Condition to check if number passed for q is indeed a prime. It is planned to change this in later versions to something like fermat algorith or miller-rabin, to improve perforamnce
(if (coprime (* p q) e) (progn																	; Condition to check if number passed for e is coprime of the n=(p*q)
	(defvar primelist)																			; Variable to hold all the prime numbers
	(defvar n)																					; n parameter of the RSA algorithm
	(defvar totient)																			; totient. Calculated as (p-1)*(q-1)
	(defvar coprimelist)																		; variable that will hold all coprime numbers
	(defvar d)																					; d parameter of the RSA algorithm, or modular multiplicative inverse
	(defvar dcmessage)																			; variable to hold an decrypted message
	(defvar rsabyte)																			; variable that will hold each separate byte that will be decrypted
	(setf dcmessage '(nil))																		; Setting the decrypted message initially to NIL
	(setf bytes (string-to-bytes bytes))														; Convert an encypted message to it's byte representation
	(setf n (* p q))																			; Compute n as p*q
	(setf totient (lcm (- p 1) (- q 1)))														; Calculate totient
	(setf d (modular-multiplicative-inverse e totient))											; Calculate modular multiplicative inverse for e and totient
	(loop for mbyte in bytes																	; For loop that goes through each byte in the list
	 do (progn (setf rsabyte (rem (expt mbyte d) n)) (setf dcmessage (cons rsabyte dcmessage)))	; Decryption based on the public key (n, d) is applied to the encrypted message
	)
	 (setf dcmessage (remove nil dcmessage))													; Remove NIL from the message
	 (setf dcmessage (reverse dcmessage))														; Since each newly decrypted character was pushed on top of the list, reverse list order
	 (setf dcmessage (mapcar #'code-char dcmessage))											; Map bytes to their appropriate character values
	 (setf dcmessage (coerce dcmessage 'string))												; Form a string out of the newly mapped characters
	 (return-from rsa-decrypt-kzi dcmessage)													; Return decrypted message
) (return-from rsa-encrypt-kzi "Value for e is not a coprime of totient (p*q)")					; Print out an error message if e is not coprime of n=(p*q)
) (return-from rsa-encrypt-kzi "Value for q is not a prime number")								; Print out an error message if q is not a prime number
) (return-from rsa-encrypt-kzi "Value for p is not a prime number")								; Print out an error message if p is not a prime number
))

