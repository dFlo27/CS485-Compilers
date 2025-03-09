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

Philosophy of the program:

Why Hash tables?

I used Hash tables extensively because I can avoid directly handling any lists or types. To add or remove an element from a Hash table, I can simply call
Hashtbl.add, .find, or .remove add the return value will be unit. Another benefit of Hash tables is the immutability of elements. When reading expressions
type checking, I can add any object without worrying about losing the old value.

Why store only this much information?

This program relies heavily on the AST for type checking and printing to .cl-type. I decided that instead of trying to save every bit of info of the AST, I
can rather store essential info like types and use pointers when more information is needed. This does make the program longer because of requiring multiple
passes through the AST. But, this is a sacrifice I'm willing to make.

Why use exceptions to handle type-check errors?

So I can shove all of the error handling to the end of the program. It looks cleaner that way and it helps me avoid making small mistakes to the error 
messages that could go unnoticed.

Structure of the program:

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

Function read_features does just as it names implies, it read features. 
    The function will first read three lines of the AST, retreiving the type of feature, 
its location, and its identifier. 
    Then there are two cases, one for attributes and another for method. 
1) Attributes -> 
    The function first checks if the feature name is self and raises an exception if so. Then it will check if it has already been defined 
in one of its parents using the function attribute_has_ancestor. Then the function will read from the AST to get the type. If the type is undefined, 
another exception. If the attribute has an initializer, the program skips it using expressions_skip. 
2) Methods -> 
    The program checks if the method has been defined previously in the class, then it will get the formal length and make a new a_method
object. The program will also try to find a parent version of the method. When creating the new a_method object, the program will check the formal arguments
for any self variable name and for duplicates. The return type will also be checked to see if its defined. If the method was previously defined in a parent
class, then the program will check if the formal types and return types are the same.

Section Three: Adding Expressions to Attributes and Methods

After adding the features to every class, the program has enough info to type-check expressions. The program does this by iterating through the Hashtbl 
classes while calling the function add_expressions. add_expressions jumps to the location of the class's features if the class is not a standard class,
(Object, etc.) and calls another function, type_check_features.

type_check_features will type check each feature defined in the class. The function reads from the AST three lines just like read_features. There are three
cases for type_check_features:
1. attribute_no_init -> 
    Read and ignore the type of the attribute
2. attribute_init    -> 
    Read and ignore the type of the attribute,
    Call read_expression to get the dynamic type of the attribute,
    Check if the dynamic type conforms to the static type using conforms_to
3. method -> 
    Retreive the method from the class method_env,
    Add each formal parameter to attribute_env,
    Reads and discards the method type,
    Call read_expression to get the dynamic type, 
    Checks if dynamic type conforms to static type using conforms_to 
    and finally remove the formal parameters from attribute_env.

conforms_to:
    If the parent class is a SELF_TYPE, then the child class must be SELF_TYPE
    If child type is a SELF_TYPE, then the parent class should be either the class SELF_TYPE is referring to or a parent of that class
    If neither is a SELF_TYPE, then recursively:
        get child class parent
        if the child's parent is the parent class then return true
        else if the child's parent is Object then return false
        else the continue the recursive descent with child's parent

read_expression:
    This function reads expressions. More specifically, it retreives the dynamic type of expressions using the information the program scrouged up. 
    The function first reads the porgram line and keyword of the function. Then depending on the keyword the function will do the following steps:
    assign -> Checks if variable is called self. Checks if variable exists. Checks if type exists. Checks if type conforms to variable static type
    block  -> No checking
    case   -> For each variable in branch, check if its called self, check if type is SELF_TYPE, check if type exists, check if type is duplicated, and finds
        least upper bound of all branches
    dynamic_dispatch -> Checks if method exists. Checks if argument length matches with formal length. Checks if arguments conform to formal
    self_dispatch    -> Does the exact same checkings as dynamic_dispatch
    static_dispatch  -> Checks if parent exists. The rest is the same as dynamic_dispatch
    eq, lt, le -> Compare both sides and check if they are equal.
    identifier -> Check if name is self and check if variable exists.
    if         -> Checks conditional expression if its dynamic type is bool
    integer, string, isvoid, true, false -> No type checking
    let -> For each let binding, check if variable is called self. Check if let binding is SELF_TYPE. If binding has an initializer, check dynamic type
        conforms to static type
    negate -> Check if dynamic type is Bool
    new -> Check if type/class exists
    Arithmetic Operations -> Check if both sides are Int
    while -> Check if conditional is boolean

    The final list will be the all of the types of each keyword encountered.

Section Four: Printing Galore

The final part of the program prints the results of the previous sections to a .cl-type file. This section is split into four parts:
1) class_map
2) implementation_map
3) parent_map
4) Annotated AST

The first part will print all of the defined and inherited attributes of each class according to the location of its definition. 
The program performs this task by first sorting all classes in alphabetical order.
Then for each class, it will:
    print class name and number of attributes,
    call print_attributes with a list of attributes

print_attributes:
    If attribute is initialized, print initializer else no_initializer
    Jump to attribute location within the AST
    Read and print attribute name
    Read and print type of attribute
    If attribute has initializer, call print_expressions to print the dynamic type and expressions together

The implementation_map is very similar to class_map. Instead of attributes, the program prints out the methods of each class.
The program will again print the class name and now the number of methods. The classes are sorted alphabetically too.
Then print_methods is called.

print_methods has two cases due to how base/standard classes are defined in the program. 
print_methods:
1)  Class is one of the standard ones (Object, IO, etc.):
    print method name and number of parameters,
    then print the formal parameter if the method is either concat, in_int, in_string, or substr.
    print class name again and print 'internal'
    Finally, print the class name, period '.',  and method name all together in one line.
2) Class is defined explicitly in the COOL program:
    Jump to the method location within the AST
    Read and print method name
    Read and print the number of method formals
    Then print the formal names each in a seperate line while discarding the locations of the formal and its type
    Read and discard method type and location
    Print the class name of where its defined
    Call print_expressions to print the dynamic type and expressions together

The parent map is very simple, the program prints all of the class names and their parents. The Object class is not included 
for it is the only class with no parents. The classes are printed according to alphabetical order.

The Annotated AST is the same as the AST from the input with the inclusion of types for every expression. 

The program jumps to the beginning of the AST,
prints the number of classes defined,
calls print_annotated_ast

Prints class information
print_annotated_ast:
    print class location and name,
    print inherits keyword and parent if it does inherits from another class
    print feature length,
    call print_features
    continue to next class

Print feature information
print_features: 
    Very similar to read_features so I'll be breif.
    print feature keyword and name
    if feature is attribute, print attribute type and location
        if attribute has initialization, call print_expressions
    if feature is method, print formal parameter length
    print formals
    call print_expressions

print_expressions:
    Like read_expressions, but instead of making a type list, the program reads it.
    In general, the program prints the location of the expression and prints the dynamic type of the expression using List.hd type_list.
    Afterwards, the program follows read_expression structurally and prints whatever line it reads.










...
Thank you for reading, I put too much time into this