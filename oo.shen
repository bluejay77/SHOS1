\* Copyright (c) 2013, Mark Tarver, and (c) 2016 Antti Ylikoski

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. The name of Mark Tarver may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Mark Tarver ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mark Tarver BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED 
AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Edited by Dr Antti Ylikoski:

Closing paren problem corrected AJY 2016-05-27

Improved on the formatting AJY 2016-05-27

***** The name of Dr Mark Tarver shall not be understood or
interpreted as an endorsement of this software product in any manner
whatsoever.  AJY 2016-05-27

Augmented the design of this OO library by adding the functions
make-instance; iget; and iput.  AJY 2016-05-27

Authored the additional functionality
make-instance; iget; and iput.  AJY 2016-05-26


*\



\*


(6+) (defclass mammal ()
               blood symbol warm)
mammal : symbol

We create a class of mammals with the attribute blood which takes an
object of type symbol set by default to warm.

(7+) (defclass bird (mammal)
               flies boolean true,
               latin_name string,
               migrant boolean)
bird : symbol

We create a class of birds which are a subclass of mammals with a
boolean attribute flies set by default to true. with a Latin name
which is a string and a migrant attribute which is boolean.


(8+) (instance bird)
[[flies true] [latin_name] [migrant] [blood warm]] : (instance bird)

The instance created here is temporary so you would have to assign it
to some global variable to keep it. The actual class definitions are
stored on property lists so they are permanent.


(9+) (assign (instance bird) latin_name "passer domesticus")
[[flies true] [latin_name "passer domesticus"] [migrant] [blood warm]]
: (instance bird) 

Assign a value to an instance of bird.

(10+) (assign (instance bird) migrant maybe)
type error

The assignment fails because the value is not a boolean.


*\


(package oo [defclass defclass-macro assign instance attribute class
              make-instance iget iput object
	      cl-slots cl-parents cl-name process-slots
	      attribute-type class?
	      object
	      slots parents class-name instance-slots ]
	      

(defmacro defclass-macro
  [defclass Class \\ class name, a symbol
            SuperClasses \\ the list of super classes
            | Slots] \\ the slots, by the function process-slots
  -> (create-class
      Class
      SuperClasses
      (process-slots Class Slots)))


(define process-slots
  Class [Attribute Type Value] -> \\ one slot
      (do (put Class Attribute Type) \\ stored as properties
          [[Attribute Value]]) \\ return list
  Class [Attribute Type] -> \\ one slot here
  (do (put Class Attribute Type) [[Attribute]])
  Class [Attribute Type Value , | Slots] -> \\ list of slots
      (do (put Class Attribute Type)
          [[Attribute Value] | (process-slots Class Slots)]) \\ recurse
  Class [Attribute Type , | Slots] -> \\ list of slots?
      (do (put Class Attribute Type)
          [[Attribute] | (process-slots Class Slots)]) \\ recurse
  Class [] -> [[]] \\ no slots, or, end of recursion
        Class _  -> \\ otherwise: there is a syntax error
      (error "syntax error in class definition of ~A~%" Class))

(declare instance [[class Class] --> [instance Class]])

(datatype o-o

  if (class? Class)
  ______________________
  Class : (class Class);

  if (= (attribute-type Class Attribute) Type)
  __________________________________
  Attribute : (attribute Class Type);
  
  Instance : (instance Class);
  Value : A;
  Attribute : (attribute Class A);
  _____________________________________________________ 
  (assign Instance Attribute Value) : (instance Class);)
  
(define attribute-type
  Class Attribute -> (trap-error (get Class Attribute) (/. E [])))     
  
(define class?
  Class -> (cons? (trap-error (get Class slots) (/. E false))))  

(define create-class
  Class SuperClasses Slots ->
      (let SuperSlots (mapcan (function instance) SuperClasses)
           ClassSlots (append Slots SuperSlots)
           Create (put Class slots ClassSlots) \\ property slots: list
	   Paren (put Class parents SuperClasses) \\ super i e parents
	   Name (put Class class-name Class) \\ her name
           Class))

\\ instance creates an instance of the class,
\\ which is a list of lists
(define instance
  Class -> (get Class slots))

\\ assign assigns the value of a slot of an instance
(define assign
  [[Attribute | _] | Slots] Attribute Value ->
      [[Attribute Value] | Slots]
  [Slot | Slots] Attribute Value ->
      [Slot | (assign Slots Attribute Value)])

\*

Classes are symbols.

The attributes have been stored with properties.  The properties are
the properties of the class symbol.

Instances of classes are also symbols; their slots lists are on their
instanceSlots property, of the class instance symbol.

The function (make-instance Class) makes an instance.

This list of lists is the value of the instanceSlots property of the
class instance symbol.

The class instance symbol is returned, by the function (make-instance).

The properties of a class symbol are:

* her slots, which are the classes own slots, and also its
superclasses

* parents, the immediate superclasses

* children, if the class has ones

* class-name, the name of the class


*\


(define cl-slots
    Class -> (get Class slots))

(define cl-parents
    Class -> (get Class parents))

(define cl-name
    Class -> (get Class class-name))

\*

The function (make-instance Class) makes an instance.

The instance symbol is a gensym.

This list of lists is the value of the instance-slots property of the
instance symbol.

Usage: e g

(set i (make-instance bird)) \\ Value of i is a symbol, a gensym

Then e g:

(iput (value i) latin_name "Haliaetus leukokephalos")

And e g:

(iget (value i) latin_name)


*\

(define make-instance
    Class ->
    (let
        InstanceName (gensym instance-)
        Ignore (put InstanceName instance-slots (instance Class))
	InstanceName))
\*

The function (iget InstanceName SlotName)

will get the slot SlotName from the symbol InstanceName

*\

(define iget
    InstanceName SlotName ->
    (assoc SlotName (get InstanceName instance-slots)))


\*

The function (iput InstanceName SlotName)

will assign the slot SlotName of the symbol InstanceName into the
value Val

*\

(define iput
    InstanceName SlotName Val ->
    (let
        Lst (get InstanceName instance-slots)
	NewLst (assign Lst SlotName Val)
	(put InstanceName instance-slots NewLst)))





) \\ End package (fixed by AJY 2016-05-27)


