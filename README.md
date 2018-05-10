# BE-Tree

![Build Status]( https://travis-ci.org/FrankBro/be-tree.svg?branch=master "Build Status")

## Questions
* For strings, should we use the q-gram thing mentionned in the paper, or make the splitting strategy different (ie. if start by. there might never be an bucket of a single value doe, unless we configure like "split max for 2 letters, etc)? Could also use all the expressions to make a hash table and split this way, since we would never match on anything beside some strings if we always do exact string matches + one extra value for anything else for non-equality.

## Implementation
* For variables that allow undefined, keep in mind that `match_be_tree` will always need to go down pnode's with those variables. Therefore, we rank them worse in the scoring part because they will cause a bunch of useless evaluations.
* Right now, strings won't ever be chosen for becoming a pnode since we can't really split them on their domain.

## TODO
* Allow a way to mark string values as if we know all of the possible values and they could therefore be split.
* Allow a way to control the splitting of float values. Right now it splits like integers but that won't work well for values that have a small domain (eg -0.01 to 0.01). Use domain to find a good split
* In match expr, don't search for variable ids, know which they are before hand
* match_be_tree, config should be const but we don't have a way to get a variable id without also creating if it doesn't exist. Fix that
* When we are within a "not" expression, we would technically need to invert all the ranges inside, if that's even possible
