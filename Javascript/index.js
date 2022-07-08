// setTimeout(() => console.log("abc"), 3000);

// Object methods
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

console.log(math.sum(2,5));
console.log(math.mult(2,5));
console.log(math.div(5,2));
console.log(math.pi);


