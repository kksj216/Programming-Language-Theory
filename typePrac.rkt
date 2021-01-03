#lang plai
(define-type Animal
   [bear (name string?)
         (gender string?)
         (age number?)]
   [tiger (name string?)
          (gender string?)
          (age number?)]
   [rabbit (name string?)
           (gender string?)
           (age number?)
           (child_number number?)])

(define myBear (bear "hanry" "m" 5))
(Animal? myBear)
(bear? myBear)
(rabbit? myBear)

(define myRabbit(rabbit "poly" "f" 4 5))

(define (show-age ins)
  (bear-age ins))

(show-age myBear)


(define (get-gender name1 name2)
  (type-case Animal name1
    [bear (n g a) g]
    [tiger (n g a) g]
    [rabbit (n g a c_n) g])
  (type-case Animal name2
    [bear (n g a) g]
    [tiger (n g a) g]
    [rabbit (n g a c_n) g]))

(get-gender myBear myRabbit)




(define-type GUI
	[label 		(text string?)]
	[button 	(text string?)
				(enabled? boolean?)]
	[choice	(item (listof string?))
				(selected integer?)])

(define (read-screen g)
	(type-case GUI g
		[label (t)		(list t)]
		[button (t e?)	(list t)]
		[choice	(i s) i]))

(define ch (choice '("Apple" "Strawberry" "Banana") 0))
(choice? ch)
(choice-selected ch) ; [variant_id]-[field_id]
(read-screen ch)



(define (tr (item (listof string)))
  item)

(test (tr '("a" "b")) '("a" "b")) 