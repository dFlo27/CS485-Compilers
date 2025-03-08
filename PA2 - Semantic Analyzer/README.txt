Hi, I'm back at 2am to give you amazing context on my not so great code.

To begin I'll explain the basic structure I used to store the classes of the AST.

1) object_id - This data structure represents anything with a type attached to it. Attributes, let bindings, case branches, etc. 
What it contains?
    obj_type : string -> This stores the static type of whatever it is representing
    t_list   : string list -> This stores the dynamic types of expressions. Useful for attribute initialization and methods. 
                                The head of the list is the object's static type.
    ic_pointer : int -> This is a pointer to the location of where said object is defined. Useful for jumping from one place of the AST to another

2) a_method - This data structure represents... a method. It is an extension of the object_id.
What does it contain?
    f_parameters : object_id list -> This represents formal parameters. You can notice that it is made up of object_ids
    info         : object_id      -> This represents the method itself. Contains the types of the method.

3) a_class - This data structure represents the COOL class. Exciting
    parent : string -> It's the parent of the class. Either Object or some other class 
    attribute_env : Hashtbl.(string, object_id) -> Represents the type environment for objects. This will be used throughout the AST to simulate
                                                    variable modifications (let, case, arguments)
    method_env    : Hashtbl.(string, a_method)  -> Represents the type environment for methods.
    ic_pointers   : (int, int)  -> Pointers to beginning of class in AST and beginning of features in AST

The classes are also stored in a Hashtbl called classes, so thats fun.

Here is the basic ordering of the program:

1st: Read Class info:   Names, Parents, and ic_pointers to where it is
2nd: Read Features:     Get the static types of each feature and make sure there aren't any rule violations
3rd: Read Expressions:  For each attribute with an initializer and method read the expression associated to it. Check for any type errors.
4th: Print to .cl-type: Print class_map, implementation_map, parent_map, and annotated AST.

This is the ordering I followed when designing this program. Below is a more in-depth explanation as to how the program functions.

First Section of the Program: Adding classes to classes :)

The program starts with adding defined classes to the Hashtbl classes. This is done by going through the entire AST and only reading basic class info.
That'll be the class's name, who it inherits from, and its location in the COOL program. While the program does this it will check for any classes that are
called "SELF_TYPE" and raise an exception if so. I'll talk about exceptions later. (add_classes function does this)

Afterwards, the program will check if the class Main is defined. If not, another exception will be raised. Then, the program will check for any unknown
parents that hasn't been defined in the program. Finally, the classes are sorted in topological order, generating a list that future methods will use to 
traverse the AST safely

Second Section of the Program: Introducing Features to classes
 
The program will now use the path to properly retreive the features of each class. The program calls add_features with the path and for each defined class
the function will:
    go to the feature list (using the class's ic_pointers)
    call another function read_features
    and if the class is main, check if method main is defined with no parameters.

Function read_features does just as it names implies, it read features. The function will first read three lines of the AST, retreiving the type of feature, 
its location, and its identifier. Then there are two cases, one for attributes and another for method. 
For attributes, the function first checks if the feature name is self and raises an exception if so. Then it will check if it has already been defined 
in one of its parents using the function attribute_has_ancestor. Then the function will read from the AST to get the type. If the type is undefined, 
another exception. If the attribute has an initializer, the program skips it using expressions_skip. 
The method case has the program checking if the method has been defined previously in the class, then it will get the formal length and make a new a_method
object. The program will also try to find a parent version of the method. When creating the new a_method object, the program will check the formal arguments
for any self variable name and for duplicates. The return type will also be checked to see if its defined. If the method was previously defined in a parent
class, then the program will check if the formal types and return types are the same.

Section Three: Adding Expressions to Attributes and Methods

After adding the features to every class, the program has enough info to type-check expressions. The program does this by iterating through the Hashtbl 
classes while calling the function add_expressions. add_expressions 