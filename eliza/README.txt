Names: Sorin Cho and Ivan Zhao

ID's: scho31 and izhao3
---------------------------------------------------------------------------------------------------
Instructions: The user would input a sentence into the terminal. If the sentence fits in with
one of the rules that we defined in the rules.ml, it would receive the corresponding response based 
off of the phrases that that it pulls out. Eliza would then create a response based off the phrases
that it by extracting the corresponding wild card phrases uses the extract procedure and creating a response using
the make response procedure. It'll output the outcome of make_response. If it doesn't match anything, it'll 
match the catch all case "Any" and return the sentence "Howdy partner, how ya doing?"

---------------------------------------------------------------------------------------------------

Overview of how all the pieces fit together:

The code is written in three main parts: extract, make_response, and eliza_respond. In order to figure out
how the pieces fit together, we should break down exactly what happens.

____________________________________________EXTRACT______________________________________________________________

At the start, the code eliza takes the input and changes it from a sentence in standard 
structure to a phrase. In extract, it takes this phrase (which we call the input). Extract has three main components.
Firstly, we check the literal case. If the first element of the input list matches the first word of the pattern list
then we recursively call extract on the rest of the list because we only want the words that are "wild cards" per se in
the input list.

Next, if the element in the pattern is One (and not a Lit), we then recursively call extract on the rest of
both lists. If the rest of the elements produce the phrase list option none, then the call produces none.
Otherwise, it'll match with Some(x) and produce Some([first]::x) which adds the first element to the rest of the elements
that also matched with Some. This is to ensure that we make sure that only one element is given for the pattern "One" and not
multiple elements or less then one element.


Case 1) Going through all the recursive calls:

If we're matching with the input with an Any, we first recursively call with the entirety of the input list, and the rest of the 
pattern. If it produces None, we recursively call on extract with the rest of the input and the entirety of the pattern with Any.
As such, we check if that matches with any of the left hand cases. If it produces none, then the code should return none (which
essentially means that it didn't follow any of the rules given). If it returns Some, this means that we hit the "end" of the entire 
Any list and we return a list of all the elements that correspond to the wild card any.

Case 2) Going through two recursive calls

If, when we're matching rest1 with Any::rest2 and it produces a list, we add the first element of the input (the part of the input 
that corresponds to the wild card Any and add that to the rest of the input that corresponds to the other parts of the input that are 
then extracted (such as having literals or One's in the rest of the pattern). If the next case is Any and we match with an empty case, then we just
return the first element in the input.

Case 3) Going through one recursive call

if instead we match during the first recursive call, this just means that there was no element in the Any case and we instead cons the 
empty list to the rest of the matched inputs (which could be other one's or anys)

At the end of this, we have the last base case where it's an empty list with an Any where it'll account for when Any needs to be matched
with the empty list and the match expression within this case is to also account for multiple Anys matching with the empty list. 
Whenever Any is matched with empty, it'll cons the empty list to the rest of the extract.
When both lists are empty, it'll produce Some [] because it means that all of the rules have been met and serves as starting point to build
the extract from. And we have a catch all case which returns None since it did not match with anything.

___________________________________________make_response_________________________________________________________________________

Make response essentially takes an extraction and a template and matches the two together. If the template is empty, it'll
produce the empty list. If the template is Text(x) as its first item, it'll append x to the rest of the recursive call. And if 
the first element in the template is Place(x), we take the x-1 place element and append that to the recursive call of make_response 
with the rest of the list. This will keep going until we reach the base case with an empty template.

___________________________________________eliza_respond___________________________________________________________________________

Eliza_respond takes a phrase and a rule list and so essentially we have one base case where the rules list goes to empty. The pupose
of eliza_Respond is to match the corresponding rule in the rule_list. What the code does is that we take the rule and call extract on
the input and a, which is the pattern of the corresponding rule. If they match, we make a response using the extracted list and b, the 
template. We return the output of make_response which is what we see as the output of our code. 



-----------------------------------------------------------------------------------------------------------------------------------
Hopefully there are no bugs. I will be really sad if there are bugs that I missed :(

List of people:
Tim Wang
Kushagra Agarwal

No extra features


