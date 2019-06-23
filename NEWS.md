# tag 0.2.0

# tag 0.1.0

* A new system of pattern helpers has been designed, we use `CALL(eval = TRUE)`
or `CALL(eval = FALSE)` instead of `eval.parent(CALL)` or `CALL`. Eliminating
the weird substitution that we had makes things much clearer.
* The environment play has been debugged as the code was very easy to break
* `tag_adverb` and `as_tag_adverb` are not exported anymore, they added
confusion and little value, we focus on tags
* the parameter `rm_args` was removed from `tag` and `tag_adverb` as it was never
used in package *tags* and seemed not so useful
* the parameter `eval_args` was added to be able to use lazy evaluation on tag arguments
* The README was completely reworked
* The documentation was improved
