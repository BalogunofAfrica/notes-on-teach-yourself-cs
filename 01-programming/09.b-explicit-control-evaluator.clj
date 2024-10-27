
"We have seen from the previous section that we can organize a register machine into 3 parts namely:
   1 - Data paths (registers and the data)
   2 - Finite state controller
   3 - Stack
 
 We can also structure our eval apply loop in terms of a register machine. Wed' have 7 registers namely:
  1 - EXP      : for the expression we want to evaluate
  2 - ENV      : for the environment in which this evaluation would take place
  3 - FUN      : for the procedure to apply to get our result from this expression
  4 - ARGL     : for the argument list for the procedure above
  5 - CONTINUE : for keepimng track of where we are in this eval-apply loop
  6 - UNEV     : a temporary register we can use for intermidiate values
  7 - VAL      : the register that contains the result of this expression/evaluation
"

"You could have a recursive procedure that is in fact iterative and does not need to store imtermidiate values in the stack.
 Rather it uses reduction to represent all imtermidiate values, an example would be an iterative factorial like so:"

(defn iterative-factorial
  [n]
  (letfn [(iter [product count]
            (if (> count n) product (iter (* count product) (inc count))))]
    (iter 1 1)))
"As opposed to a recursive implementation:"

(defn recursive-factorial
  [n]
  (if (zero? n) 1 (* n (recursive-factorial (dec n)))))

(recursive-factorial 6)

"Within the body of the iterative-factorial we see that we define another procedure which is then invoked immediately with initial values (1 1)
 In the else case we alway increase the product by multiplying and increase the count by adding, this is what enables the reduction and allows us
 to avoid storing intermidiate values on the stack.

 In contrast, the recursive-factorial has to store every value along the step in the stack and then unwind to carry out the multiplication to get the final answer
 "