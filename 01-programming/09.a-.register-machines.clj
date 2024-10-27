
(defn remainder [n d] (if (< n d) n (remainder (- n d) d)))

(defn gcd [a b] (if (zero? b) (gcd b (remainder a b)) a))

(comment
  "Pseudo language for a gcd register machine"
  (define-machine gcd
                  (register a b t)
                  (controller :loop
                              (branch (zero? (fetch b)) (goto :done))
                              (assign t (remainder (fetch a) (fetch b)))
                              (assign a (fetch b))
                              (assign b (fetch t))
                              (goto :loop)
                              :done))
  "This machine does an iterative process by first registering the values for the gcd a and b, and a temporary value t which would be used for swapping"
  "After registering these values, we go through our controller to solve for the problem at hand by iteratively evaluating the body till we get an answer"
    "We first off check in a branch if the value in the register b is zero, if so we are done with this solution and can exit at this point"
  "Else we assign t to be the remainder of the division of the value in register a by the value in register b. Then we follow up by assigning register a to be the value in register b"
    "And then register b to be the value in register t, then with the register having these new values we jump to the loop body and evaluate the expression again till we are done")



(defn factorial [n] (if (zero? n) 1 (* n (factorial (dec n)))))

(comment
  "Pseudo language for a factorial register machine"
    (define-machine factorial
                    (register continue n val)
                    (controller (assign continue :done)
                                :loop
                                (branch (= 1 (fetch n)) (goto :base))
                                (save continue)
                                (save n)
                                (assign n (dec (fetch n)))
                                (assign continue :aft)
                                (goto :loop)
                                :aft
                                (restore n)
                                (restore continue)
                                (assign val (* (fetch n) (fetch val)))
                                (goto (fetch continue))
                                :base
                                (assign val (fetch n))
                                (goto (fetch continue))
                                :done))
  "For this machine we introduce a way in which we can remeber the value of the previous value we have as we evaluate the body of our controller. This thing we introduce is called a `stack`"
    "It would serve as a place we can store previous answers to our question, which we have computed. The reason why we need to store the previous answer unless our earlier register machine is"
  "That the inner computed value is used to carry out some operation within the outer scope. In the case of factorial, the inner computation is multiplied within the outer computation"
    "unlike the earlier example where we can just rely on the answer of the inner computation alone. So we need a way to store values that we have computed till we hit the base case, which at that point"
  "we unwind all the values to calculate our final answer. Let us step through this using a simple factorial of 3. At the beginning we register values n (number of factorial) continue (an indicator of where we are)"
    "and val which contains the final answer. Then immediately within the controller we assingn the value of continue as :done (this would be the case after we unwind and then use the values from down up to get our final answer)"
  "If we hit the base case, we jump to the base case block of which we don't here, so we go to the next line where we save the values of the continue (:done) and n (3) registers onto the stack, then we decrement n to get 2, and assign continue to the :aft block"
    "Then we go to the begining of the :loop block and re-evaluate with the new values in the register, we check the base case which we do not hit at this point (value of register n is 2) then we store the value of continue (:aft) and n (2) registers onto the stack"
  "then we decrement n again to get 1 and store in the register n, and assign continue to the :aft block, then go again to the begining of the :loop block of which this point we hit the base case then we jump to the base case. Before we jump to the base case. Let us visualize what we stored on the stack"
    "0 - :done
     1 - 3
     2 - :aft
     3 - 2"
  "We see that the last value of our stack is indeed 2 and the first is :done, now let us jump to the :base case block. In the :base case we assign to the val register the value of register n (remeber it was 1) then we go to the block stored in register continue (the value is :aft). Now we jump to the :aft block"
    "In the :aft block, we first off restore the last value for n and continue stored on the stack which according to our diagram above are 2 and :aft respectively. then we assign to the val register the product of the value in register n and the value in register val, n is 2 and remeber val was set to 1, so that is 2 * 1 which is 2"
  "Then we go to the block stored in continue register which is still :aft, we then go restore n and continue stored on the stack again which we get 3 and :done respectively. Then we assign to register val the product of value in register n (3) and register val (2) which is 6 then we go to the block stored in the continue register which is :done"
    "This marks the end of our evaluation, and outputs the final answer as the value of register val which is 6. Which means n! for n = 3 is 6.")

(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))


(comment
  "Pseudo language for a factorial register machine"
  (define-machine factorial
                  (register continue n val)
                  (controller (assign continue :fib-done)
                              :fib-loop
                              (branch (< (fetch n) 2) :immediate-answer)
                              (save continue)
                              (assign continue :aft-fib-n-1)
                              (save n)
                              (assign n (- (fetch n) 1))
                              (goto :fib-loop)
                              :aft-fib-n-1
                              (restore n)
                              (assign val (- (fetch n) 2))
                              (assign continue :aft-fib-n-2)
                              (save val)
                              (goto :fib-loop)
                              :aft-fib-n-2
                              (assign n (fetch val))
                              (restore val)
                              (restore continue)
                              (assign val (+ (fetch val) (fetch n)))
                              (goto (fetch continue))
                              :immediate-answer
                              (assign val (fetch n))
                              (goto (fetch continue))
                              :fib-done)))