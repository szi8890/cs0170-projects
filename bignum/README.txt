Explanation of representation of bignums:

We represented bignum as a list of numbers in reverse order. This is because it is hard to
iterate on a number and using a list allows the us to use cdr and car on the list in order 
to approach each number as a long multiplication and long addition problem. Using it in 
reverse order was necessary because we needed to add the ones, then the tens, then the hundreds
and similarly in the multiplication. Therefore, by starting it in reverse, we are able to
utilize this by taking the car of a list (the first item of each list) for bignum+ and we 
are able to take the car of one list and iterate through every other item of the list for 
bignum*. This also allows for carrying over to be accomplished because we're able to add it 
to the next item in the list. We represent the bignum 0 as an empty list because the procedure 
empty allows us to check it easier.


Expression a user would type:

For bignum+, if the user wanted to do 931 + 453, they would write it as

(bignum+ (quote (1 3 9)) (quote (3 5 4)))

For bignum*, if the user wanted to do 5123 * 14123, they would write is as

(bignum* (quote (3 2 1 5)) (quote (3 2 1 4 1)))

As mentioned above, we want to iterate the list backwards and so we would write the number 
backward and in a list with each digit as its own element in the list.


Overview of how all the programs fit together:

Let's start with bignum+. When a user inputs two values into bignum+, it first checks if the 
lists bignum1 bignum2 are empty. If they aren't then it will evaluate whether adding the first
number of each bignum is greater than 9. If the list is empty, first-empty will return a 0 
which accounts for trying to add lists of differing lengths. Now, if the values add up to more than
9, then we add the the remainder of the two numbers from 10 and cons that to the recursively called
bignum+. Bignum now runs with the the rest of bignum1 and the next number in the list has one added 
to it because of the carry over and rest of bignum2. At this point, rest-empty will check if the 
bignum2 is empty. If it is, it will return an empty list. Otherwise, it'll return the rest of bignum2.
Thus, it will recursively call back and go through bignum1 and bignum2 again. Now, for the other case. 
If the first element of bignum1 and bignum2 don't add up to more than 9, then we cons adding the 
first element of the the two lists together (using first-empty to check if one of the lists is empty).
and running a recursive call on the rest-empty of the two bignums. This keeps going until both lists are 
empty and we return a list that is both bignums added together.

For bignum*, it first checks if bignum4 is empty. If so, it returns an empty list (which is how
we are representing the number 0). If bignum4 is not empty, we compare the length of each bignum. If bignum3
is longer than bignum4, then we recursively call bignum4 with bignum3 because it ensures that only one of lists is
shortening. If bignum4 islonger than bignum 3, it will multiply the first element of bignum3 and bignum4 (num1 and num2). 
If it's a value greater then 9, the function will call on bignum+ to add the remainder of multiplying these two numbers together and then 
recursively call on lil-mult with num1 and the rest-empty of num2 (which still checks if its empty) while also adding 
a 0 to to carry over list to ensure that the value of the carry over is in the correct units place.
The other option is if multipliying the number together results in a number less then 9. If so, we run bignum+ on the 
the number we get from multiplying the first element of each list together, recursively calling on lil-mult with the
rest-empty of num2 and then adding a 0 to ensure that it's in the correct place. After lil-mult is finished, we return
back to bignum* where we do bignum+ of lil-mult bignum3 and bignum4 (if we did not already run it before). As such, we then
add that to (with bignum+) to recursively calling bignum* and cons-ing a 0 to that. This is because we need to make sure that 
once we keep running through each mult procedure, we need to move the list down one element. Then, we keep recursively calling
bignum* until both lists are empty.


