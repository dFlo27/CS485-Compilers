My choices were pain. I started with C. 

I stored the graph as two lists:

    Nodes - Simple list that stores one input string per element of the list. 
        Depending on the programming language, I included the # of incoming edges for the string.
    Edges - Stores the relation of each node (input string) to each other.
            Each element consists of two strings: to and from.

I used Khan's algorithm, so I did create two more lists (s, l) to store all nodes (input) that had no incoming edge (s list)
    and to store the result of computing the algorithm (l list).


Which language was the hardest?

Maybe C; I had the most trouble writing the first version of the program.

For the other programming languages, the difficulty was in learning the language rather than coding the program.

Rust was similar to C, so that was okay. Ocaml was different to any other language I used, so it was annoying but,
    I liked how the program came out.

COOL was actually alot more forgiving than I expected. I just needed to code a lot because of the lack of any standard libraries.


Why is my test case novel?

I think it is because I used a finite state machine to represent my test case. The name of each task wasn't important as I wanted 
    to test whether my programs could produce the correct result despite the test case having complex relations.