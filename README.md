# Advent of code 2024

This is my repo to track my [Advent of Code](https://adventofcode.com/2024) solutions for 2024!

This year I'm using it as an excuse/opportunity to learn OCaml. 

## Day 1

Advent of code day 1. This was fun! It's nice to have a little 
puzzle challenge. 

I'm using Claude 3.5 to teach me how to do various things in OCaml, so
treating this as an assisted run. I'm not pasting any of the problem
itself into an LLM, just using it as an easier documentation look up.
This was a simple first day, mainly just getting used to some new parts
of ocaml. I'll probably want to build some tooling that will make it
nicer to iterate on common stuff. More parsing utils.

Also to figure out is how to split these day by day. I'm not sure what
the standard dune project formatting is like.

## Day 2

These first few days really feel like shaking the cobwebs out in the stage
of getting to know a language. Yesterday I was figuring out just how to
get off the ground, now it's how to make that repeatable and workable 
throughout the month.

Today I:
- figured out adding different binaries
- figured out writing my own library utils for some common parsing tasks 
    I'm going to need. 
- figured out how to easily debug print items 
- figured out recursive fns with pattern matching for logic

part b:
- Wow that was much more painful than I expected. It took me a long time to
  figure out which elements needed to be skipped and why. In the end trying
  to skip the previous, current, and next worked. This felt a lot more 
  painful, and there's probably much cleaner approaches. 

  I also need to work on the debugging more.
