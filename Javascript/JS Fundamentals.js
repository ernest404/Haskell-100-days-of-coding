/*
you can run JavaScript in several ways:

    Using console tab of web browsers: type and execute javascript interactively 
    Using Node.js: calling node on js scripts to execute it or by launching node interactive environment(a kind of console) by calling node.To exit .exit or ctrl + c twice
    By creating web pages: And linking them to js scripts

*/
//*!---- Console.log() -----
// All modern browsers have a web console for debugging. The console.log() method is used to write messages to these consoles. For example,
console.log("Jonas"); //prints values to the console
console.log(23);

//*! ---- Variables ----
// Labeled container for storing data values

let firstName = 'Anne'; //declaring and initializing a variable.Here, firstName is a variable. It's storing "Ann". 
var x = 5; // var is used in older versions of JS.

// You declare variables in a single statement
let x = 5,
    y = 6,
    z = 7;

// Variable once declared can be called multiple times 
console.log(firstName)
console.log(firstName)
console.log(firstName)

// If you use a variable without initializing it, it will have an undefined value.
let x; // x is the name of the variable
console.log(x); // undefined because it does not contain any value.

// It is possible to change the value of the variable
let x = 5;
// To reassign a variable value we don't use let
x = 10;
// This changed value is reflected on all variable calls.

// -- Rules for Naming Variables --
/*
1. Variable names can only start with either a letter, an underscore _, or the dollar sign $
2. Variable names cannot start with numbers.
3. JavaScript is case-sensitive. So y and Y are different variables.
4. Keywords or keyword-to-be cannot be used as variable names.
*/

// The recommended way to name variables in JS is to:
// 1. Use descriptive names for variables
// 2. Use camelCaseNotation
// 3. For const variables use uppercase snake_case. ACCESS_TOKEN
let firstName = 'Bobby';

/*-- JavaScript Constants --*/
const x = 5; //initializing constants
// Once a constant is initialized we cannot change its value
x = 10; //*!error!

// you cannot declare a constant without initializing it.
const x; //*!error!
// Use const when you are sure the value won't change throughout the program.


//*! ---- Data types -----
/*There are eight basic data types in JavaScript. They are:
Data Types             Description                               Example

Primitive- only stores a single data			
String          represents textual data                     'hello', "hello world!", `Template Literals: hello ${name}`etc
			
Number          an integer or a floating-point number       3, 3.234, 3e-2 etc.
			
BigInt          represents xxbig number by appending n to it.         900719925124740999n , 1n etc.
			
Boolean         Any of two values: true or false            true and false
			
undefined       a data type whose variable is not initialized     let a;
			
null            denotes a empty/unknown value                     let a = null; (not same as NULL or Null)
			
Symbol          data type whose instances are unique and immutable.  let value = Symbol('hello'); Symbol('hello') != Symbol('hello')

non-primitive- store collections of data
Object          key-value pairs of collection of data           let student = {name: "Ernest", age: 25,.. };*/

// typeof(): Used to find the type of variable.
let javascriptIsFun = true;
typeof(javascriptIsFun);

/*JavaScript is a dynamically typed (loosely typed) language. JavaScript automatically determines the variables' data type for you.

It also means that a variable can be of one data type and later it can be changed to another data type. */

//*! ---- String and Template Literals ----
const fName = 'Ernest';
const job = 'Software Engineer';
const birthYear = 1996;
const year = 2022;
const ernest = 'I\'m ' + fName + ', a ' + (year - birthYear) + ' year old ' + job + '!';
console.log(ernest);
// Template Literals: Used to assemble several string values.
const ernestNew = `I'm ${fName}, a ${year - birthYear} year old in ${random()}.`
console.log(jonasNew)
    // We can also use back ticks for all strings.
console.log(`Just a string `)
    // Multiline strings
console.log('String with \n\
multiple \n\
lines');
// Multiline Template strings
console.log(`String
with
multiple
lines`);


// *!---- Type Conversions ----
// Implicit conversion: automatic type conversion (upcasting)
console.log('3' + 2) // "32" + as operand btn string and any other data type results in string. coercion. String operator takes precedence
console.log('4' - '2'); // 2  other operands result to numbers
// Explicit conversion: manual type conversion
Number('324'); //Any value to number.
Number('hello') //if string is not a number result will be NaN
    // You can also generate numbers from strings using parseInt(), parseFloat(), unary operator + and Math.floor()
String(324); //Any value to string.
Boolean(1) //Any value to Boolean.


//*!--- Operators ----
// is a special symbol used to perform operations on operands (values and variables)
/*
 */
// Arithmetic Operators
let x = 5;
let y = 3;
// addition
console.log('x + y = ', x + y); // 8
// subtraction
console.log('x - y = ', x - y); // 2
// multiplication
console.log('x * y = ', x * y); // 15
// division
console.log('x / y = ', x / y); // 1.6666666666666667
// modulus
console.log('x % y = ', x % y); // 2
// increment
console.log('++x = ', ++x); // x is incremented to 6 then printed
console.log('x++ = ', x++); // prints x which is equal to 6 and then increases it to 7
console.log('x = ', x); // 7
// decrement
console.log('--x = ', --x); // x is decreased to 6 then printed
console.log('x-- = ', x--); // prints x which is equal to 6 and then decreased to 5
console.log('x = ', x); // 5
//exponentiation
console.log('x ** y =', x ** y);

// -- String Operators --
const FirstName = 'Jonas';
const lastName = 'Nick';
console.log(FirstName + ' ' + lastName); //Template string is better for this operation

//-- Assignment Operators--
let x = 10 + 5; //most common assignment operator
console.log(x);
x += 10; //x = x + 10 Addition assignment operator
console.log(x);
x *= 2; //Multiplication assignment operator
console.log(x);
x++;
console.log(x);
x--;
console.log(x);

// Comparison Operator return a boolean value
// equal to operator
console.log(2 == 2); // true
console.log(2 == '2'); // true
// not equal operator
console.log(3 != 2); // true
console.log('hello' != 'Hello'); // true
// strict equal operator: true if the operands are equal and of the same type
console.log(2 === 2); // true
console.log(2 === '2'); // false
// strict not equal operator: t rue if the operands are equal but of different type or not equal at all
console.log(2 !== '2'); // true
console.log(2 !== 2); // false
// > greater than, >= greater than or equal to
// < less than, <= less than or equal to

//-- Logical Operators --
// logical AND
console.log(true && true); // true
console.log(true && false); // false
// logical OR
console.log(true || false); // true
console.log(false || false); // false
// logical NOT
console.log(!true); // false
console.log(!!true); // true

const a = 1
const b = 2
const c = 3

console.log(a && b && c); // returns first falsy value. If all values are true last value is returned, any false results in false 
// can be used to display a button when user checks a box:  isboxchecked && display_button
console.log(a || b || c); // returns the first truthy value, if all are true
// 


/**
 & Bitwise AND			
 | Bitwise OR
 */
//-- Ternary Operator --
/* ?: returns value based on the condition*/
const res = (5 > 3) ? 'success' : 'error'; // "success" Used to assign value to a variable depending on some condition.

// in: returns true if the specified property is in the object
5 in [1, 2, 3, 4, 5]

// Console has access to the variables loaded in a webpage, but any definition gets lost when page is reloaded.

// Operator Precedence : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table
const averageage = (ageJonas + ageSarah) / 2; // Brackets has the highest preceedence 
console.log(averageage)


// *!---- Comments ----
//single line
/*Multi
line*/
// Use comments to explain why you did rather than how you did something.
// Comments shouldn't be the substitute for a way to explain poorly written code in English. 
// You should always write well-structured and self-explanatory code. And, then use comments.


// Coding Challenge #1

// Data 1
let markWeight = 78;
let markHeight = 1.69;
let markBMI = markWeight / markHeight ** 2

let johnWeight = 92;
let johnHeight = 1.95;
let johnBMI = johnWeight / johnHeight ** 2

let markHigherBMI = markBMI > johnBMI;

console.log(markBMI, johnBMI, markHigherBMI)

// Data 2
markWeight = 95;
markHeight = 1.88;
markBMI = markWeight / markHeight ** 2

johnWeight = 85;
johnHeight = 1.76;
johnBMI = johnWeight / johnHeight ** 2

markHigherBMI = markBMI > johnBMI;

console.log(markBMI, johnBMI, markHigherBMI)




//*!---- if...else statement. -----
// Check if someone is old enough to take a driver 's license.

/**The if statement evaluates the condition inside the parenthesis ().
    1.If the condition is evaluated to true, the code inside the body of if {} is executed.
    2.If the condition is evaluated to false, the code inside the body of if is skipped.
 */
const age = 17;

if (age >= 18) {
    console.log('You can have your driving license 🚗')
}

/*The if..else statement evaluates the condition inside the parenthesis.

If the condition is evaluated to true,
    the code inside the body of if is executed
    the code inside the body of else is skipped from execution

If the condition is evaluated to false,
    the code inside the body of else is executed
    the code inside the body of if is skipped from execution
 */
if (age >= 18) {
    console.log('You can have your driving license 🚗')
} else {
    const yearsLeft = 18 - age
    console.log(`You are underage wait another ${yearsLeft} years`)
}

// else..if statements
/*  
If condition1 evaluates to true, the code block 1 is executed.
If condition1 evaluates to false, then condition2 is evaluated.
    If the condition2 is true, the code block 2 is executed.
    If the condition2 is false, the code block 3 is executed.
*/

yearOfBirth = Number(prompt('In which year were you born ?'))

if (yearOfBirth === 2000) {
    console.log(`Born in 21 Century !`);
} else if (yearOfBirth === 1999) {
    console.log('Born in 20th Century');
} else {
    console.log('error!!');
}

/**Nested if...else Statement
You can also use an if...else statement inside of an if...else statement. 
This is known as nested if...else statement. */
// check if the number is positive, negative or zero
const number = prompt("Enter a number: ");

if (number >= 0) {
    if (number == 0) {
        console.log("You entered number 0");
    } else {
        console.log("You entered a positive number");
    }
} else {
    console.log("You entered a negative number");
}

// In programming, loops are used to repeat a block of code.
//*! for loop

// for (initializeExpression counter; condition; incrementExpression) {
// for loop body
// }
// print a statement 5 times
for (let i = 1; i <= 5; i++) {
    console.log(`I love JavaScript.`);
}

// program to display the sum of natural numbers from 1 to 500
let sum = 0;
for (let i = 1; i <= 500; i++) {
    sum += i; // sum = sum + i
}
console.log('sum:', sum);

//Infinite loop: If the test condition in a for loop is always true, it runs forever (until memory is full).


//*! ---- while loop ----
// while (condition) {
// body of loop
// increment_counter
// }

// print statement 5 times
let i = 1
while (i <= 5) {
    console.log(`I love javascript ${i}`);
    i += 1;
}


// *! ---- do while loop ----
/**
do {
    // body of loop
} while(condition)
 */
// The body of the loop is executed at first. Then the condition is evaluated.
// Note: do...while loop is similar to the while loop. The only difference is that in do…while loop, the body of loop is executed at least once.


// print statement 5 times
let i = 1
do {
    console.log(`I love javascript ${i}`);
    i++;
} while (i <= 5)
// If the condition of a loop is always true, the loop runs for infinite times (until the memory is full).
// This is usually caused when the loop lacks an incremental Expression

// While loop is versatile it doesn't really depend on the counter thats why:
// A for loop is usually used when the number of iterations is known.
// And while and do...while loops are usually used when the number of iterations are unknown or until some condition is met.

// *! Break Statement
// The break statement is used to terminate the loop immediately when it is encountered.
// When break is used inside of two nested loops, break terminates the inner loop. 
for (let i = 1; i <= 5; i++) {
    // break condition     
    if (i == 3) {
        break;
    }
    console.log(i);
}
// when i is equal to 3, the break statement terminates the loop.
// The break statement is almost always used with decision-making statements.

// *! Continue statement
// The continue statement is used to skip the current iteration of the loop and the control flow of the program goes to the next iteration.
// The continue statement is almost always used with decision-making statements.
for (let i = 1; i <= 5; i++) {
    // break condition     
    if (i == 3) {
        continue;
    }
    console.log(i);
}
// When continue is used inside of two nested loops, continue skips the current iteration of the inner loop.
// *!---- switch statement ----
// 
const day = 'Sunday';
// The switch statement evaluates an expression and executes the corresponding body that matches the expression's result.
// The switch statement checks the value strictly. Value + type are considered.
switch (day) { //value we are testing
    case 'Monday': //if day === 'Monday'
        console.log('Plan course'); //body is executed
        break; // without break code continues executing to the next case
    case 'Tuesday':
        console.log('Create course');
        break;
    case 'Wednesday':
    case 'Thursday':
        console.log('practice course');
        console.log('Create course questions');
        break;
    case 'Friday':
        console.log('Review course');
        break;
    case 'Saturday':
        console.log('Deliver the course');
        break;
    case 'Sunday':
        console.log('Relax');
        break;
    default: // if there is no matching case the default body is executed
        console.log('Invalid Day');
        break;
}
/**
    - The break statement is optional. If the break statement is encountered, the switch statement ends.
    - If the break statement is not used, the cases after the matching case are also executed.
    - The default clause is also optional.
*/
// In a JavaScript switch statement, cases can be grouped to share the same code.
// multiple case switch program
let fruit = 'apple';
switch (fruit) {
    case 'apple':
    case 'mango':
    case 'pineapple':
        console.log(`${fruit} is a fruit.`);
        break;
    default:
        console.log(`${fruit} is not a fruit.`);
        break;
}

// *!---- Functions ----
// This is a block of code that performs a specific task. There are inbuilt and user-defined functions in Java.
// -- declaring a function --
/**
 function(keyword) nameOfFunction () {
    // function body   
}
 */
function logger() {
    console.log("My name is Jones");
}

// Function names should be descriptive.

// -- calling a function --
// To use that function, we need to call it.
logger();

// -- Functions with parameters -- 
// Functions can take values as parameters(variables specific to a function that are defined during function calls)
function fruitProcessor(apples, oranges) {
    console.log(`Juice with ${apples} apples and ${oranges} oranges.`);
}
//Functions with Defualt parameter 
function fn (a = 0, b = 0) {
    return [a , b ] 
}

console.log(fn(2));
fruitProcessor(5, 0); //These values of the parameters that are passed during function calls are called arguments.

// -- Function Return --
// The return statement is used to return the value to a function call.
// If nothing is return, the function returns an undefined value.
function add(num1, num2) {
    return num1 + num2;
}

// -- Function Expressions --
// Function as a value stored in variable
let x = function(num1, num2) {
    return num1 + num2;
}

result = x(5, 5)

// ---- Arrow Functions ----
// are anomymous
// parameter => function body
// () => function body

// Example 1
birthYear => 2021 - birthYear; //implicit return when there is no braces for function body.
// to call this function on a value
(birthYear => 2021 - birthYear)(1996)

// The right way is to store the function in a variable.This makes it easiy callable.
const calcAge3 = birthYear => 2021 - birthYear;
const age3 = calcAge3(1996);
console.log(age3);

// Example 2
const yearUntilRetirement = birthYear => {
    const age = 2021 - birthYear;
    const retirement = 65 - age;
    return retirement;
};

console.log(yearUntilRetirement(1963));

// arrow function with 2 parameters
const yearUntilRetirement = (birthYear, name) => {
    const age = 2021 - birthYear;
    const retirement = 65 - age;
    return `${name} retires in ${retirement} years`;
};

console.log(yearUntilRetirement(1963, "Mum"));
// Best for one-liner tasks
// Function call back.

// *!--- Variable scope ---

/**
 * Scope refers to the availability of variables and functions in certain parts of the code.

In JavaScript, a variable has two types of scope:

    Global Scope: A variable declared at the top of a program or outside of a function. It means it can be used anywhere.
    Local Scope: A variable that can only be accessed within a function.

    Note: It is a good practice to avoid using global variables because the value of a global variable can change in different areas in the program.
    It can introduce unknown results in the program.
 */

// Local vs Global scope Example:
let a = "hello"; //global variable

function greet() {
    let b = "World" //local variable
    console.log(a + b); // hello world
}

greet();
console.log(a + b); // error

// let is Block Scoped
// The let keyword is block-scoped (variable can be accessed only in the immediate block).

// Example:
// global variable. It can be accessed anywhere in the program
let a = 'Hello';

function greet() {

    // local variable: It can be accessed only inside the function greet
    let b = 'World';

    console.log(a + ' ' + b);

    if (b == 'World') {

        // block-scoped variable: It can be accessed only inside the if statement block
        let c = 'hello';

        console.log(a + ' ' + b + ' ' + c);
    }

    // variable c cannot be accessed here
    console.log(a + ' ' + b + ' ' + c);
}

greet();
// Note: In JavaScript, var is function scoped and let is block-scoped. If you try to use var c = 'hello'; inside the if statement in the above program, the whole program works, as c is treated as a local variable.


// Function Hoisting

// A function can be called before declaring it.
// Not possible when a function is used as an expression. Only declarations are hoisted.

// program to print the text
greet(); //Hi, there

function greet() {
    console.log('Hi, there.');
}

/* 
Note: Generally, hoisting is not performed in other programming languages like Python, C, C++, Java.

Hoisting can cause undesirable outcomes in your program. And it is best to declare variables and functions first before using them and avoid hoisting.

In the case of variables, it is better to use let than var.
*/

// *! ---- Recursion ----
// A function that calls itself is called a recursive function.
// Factorial of a number

/*function recurse() {
    if(condition) { To prevent infinite recursion, you can use if...else statement (or similar approach) where one branch makes the recursive call, and the other doesn't.
        recurse();
    }
    else {
        // stop calling recurse()
    }
  }
*/

recurse();

// Example:A program to find the factorial of a number

function factorial(x) {

    // if number is 0
    if (x === 0) {
        return 1;
    }

    // if number is positive
    else {
        return x * factorial(x - 1);
    }
}

const num = 3;

// calling factorial() if num is non-negative
if (num > 0) {
    let result = factorial(num);
    console.log(`The factorial of ${num} is ${result}`);
}

// *! ---- JS Objects ----

const person = {
    name: "Ernest",
    instructor: true
}
// order doesn't matter in objects
// Access is done by indexing with the key in quotes
console.log(person);
console.log(person['name']);

// add value
person.age = 25; //Or person["age"] = 25;
console.log(person);

// delete value
delete person.age; //Or delete person["age"]
console.log(person);

// we can modify const person because it just holds the reference to the oblect and not the object itself. Reference type.

// Get the keys of object
Object.keys(person); //an array of keys

// Get the values
Object.values(person); //an array of values




// -------- Arrays --------
// arrays are objects under the hood.  
const hobbies = ["snowboarding"];

// key as index, value as value
console.log(Object.keys(hobbies));
console.log(Object.values(hobbies));

// Accessing values
console.log(hobbies[0]); 

// Adding values
hobbies[1] = "books"

console.log(hobbies);

hobbies[5] = "running" // creates an array with empty valeus in between. Not recommend.

// the best way to add elements.

hobbies.push('singing','eating'); //multiple elements added by push.

console.log(hobbies);

// remove elements
hobbies.pop();
console.log(hobbies);

// array variables



// ---- Object methods -----
const math = {
    sum: (a,b) => a+b,

    mult: function (a,b) {
        return a*b
    }, 

    div(a,b){
        return a/b
    }, 
    pi: Math.PI}

// methods are keys functions assigned to them
// attributes/properties are keys assigned to values.

console.log(math.sum(2,5));
console.log(math.mult(2,5));
console.log(math.div(5,2));

console.log(math.pi);



// Data 1
let markWeight = 78;
let markHeight = 1.69;
let markBMI = markWeight / markHeight ** 2

let johnWeight = 92;
let johnHeight = 1.95;
let johnBMI = johnWeight / johnHeight ** 2

if (markBMI > johnBMI) {
    console.log(`Mark's BMI (${markBMI}) is greater than John's (${johnBMI})`)
} else {
    console.log(`Mark's BMI (${markBMI}) is less than John's (${johnBMI})`)
}


// Check if someone has money

const money = 0;

if (money) {
    console.log('Funds available')
} else {
    console.log('Not funds available')
}

// always use strict equality even if we need type conversion.

const favorite = prompt('What is your favorite number?')
console.log(favorite); // favorite is a string value.
console.log(typeof(favorite))

// Take input as a number type
const favoriteNum = Number(prompt('What is your favorite number?'))
console.log(favoriteNum); // favorite is a Numeric value.
console.log(typeof(favoriteNum))

// else if 
yearOfBirth = Number(prompt('In which year were you born ?'))

if (yearOfBirth === 2000) {
    console.log(`Born in 21 Century !`);
} else if (yearOfBirth === 1999) {
    console.log('Born in 20th Century');
} else {
    console.log('error!!');
}

// Not equal to !== strict Vs != loose Always use strict version of operator.

// Boolean Logic Operators: AND, OR & NOT.

const hasDriversLicense = true
const hasGoodVision = true

console.log(hasDriversLicense && hasGoodVision);
console.log(true || false);
console.log(true && !(true));

// Coding Challenge

let dolphinesAvg = (96 + 108 + 89) / 3;
let koalasAvg = (88 + 91 + 110) / 3;

if (dolphinesAvg > koalasAvg) {
    console.log("Dophines win");
} else if (dolphinesAvg < koalasAvg) {
    console.log("Koalas win");
} else {
    console.log("Draw");
}

// Bonus

dolphinesAvg = (97 + 112 + 101) / 3;
koalasAvg = (109 + 95 + 123) / 3;

if (dolphinesAvg > 100 && dolphinesAvg > koalasAvg) {
    console.log("Dophines win");
} else if (koalasAvg > 100 && dolphinesAvg < koalasAvg) {
    console.log("Koalas win");
} else if (dolphinesAvg > 100 && koalasAvg > 100 && dolphinesAvg === koalasAvg) {
    console.log("Draw");
} else {
    console.log("Minimum score not achieved no Team wins");
}

dolphinesAvg = (97 + 112 + 101) / 3;
koalasAvg = (109 + 95 + 106) / 3;

if (dolphinesAvg > 100 && dolphinesAvg > koalasAvg) {
    console.log("Dophines win");
} else if (koalasAvg > 100 && dolphinesAvg < koalasAvg) {
    console.log("Koalas win");
} else if (dolphinesAvg > 100 && koalasAvg > 100 && dolphinesAvg === koalasAvg) {
    console.log("Draw");
} else {
    console.log("Minimum score not achieved no Team wins");
}

// Switch statement: Complicated alternative to ifelse.

const day = 'Sunday';

switch (day) { //value we are testing
    case 'Monday': // day === 'Monday'
        console.log('Plan course');
        break; // without break code continues executing to the next case
    case 'Tuesday':
        console.log('Create course');
        break;
    case 'Wednesday':
    case 'Thursday':
        console.log('practice course');
        console.log('Create course questions');
        break;
    case 'Friday':
        console.log('Review course');
        break;
    case 'Saturday':
        console.log('Deliver the course');
        break;
    case 'Sunday':
        console.log('Relax');
        break;
    default:
        console.log('Invalid Day');
        break;
}

if (day === 'Monday') {
    console.log('Plan course');
} else if (day === 'Tuesday') {
    console.log('Create course');
} else if (day === 'Wednesday' || day === 'Thursday') {
    console.log('Practice course');
    console.log('Create course questions');
} else if (day === 'Friday') {
    console.log('Review course');
} else if (day === 'Saturday') {
    console.log('Deliver course');
} else if (day === 'Sunday') {
    console.log('Relax');
} else {
    console.log('Invalid Day');
}

// Statements vs expression

// Expressions statements that produce a value, operations, values are also expressions.
// Statements a bigger piece of code that does not produce any value. translate actions.
// An expression is something, while a statement does something.

// conditional(Ternary) operator ? is if, : is else
const age = 23;
age >= 18 ? console.log('I can drink alcohol') : console.log("I can't drink alcohol");

// Can be used to conditionally declare a variable
const drink = age >= 18 ? 'alcohol' : "water";
console.log(drink);

// if else implementation

let drink2;
if (age >= 18) {
    drink2 = 'alcohol'
} else {
    drink2 = 'water'
}
console.log(drink2);

console.log(`I like to drink ${age >= 18 ? 'alcohol' : "water"}`);

// Perfect for small and quick decisions.Not applicable for large blocks of code.

// Code challenge.
const billValue = 240;
const tip = billValue >= 50 && billValue <= 300 ? billValue * 0.15 : billValue * 0.2
console.log(`The bill was ${billValue}, the tip was ${billValue >= 50 && billValue <= 300 ? billValue * 0.15 : billValue * 0.2} and the total value ${billValue + tip}`);

// Falsy values: 0, "", null, undefined