// For Defualt export, we can you can choose the name of the value your are importing.
import person from person.js
import ps from './person.js' //name in the import file is up to you.

console.log(person); 
console.log(ps); 


// For Named export, name is defined by export but you can use alias as to assign a different name.
import { add } from './utility.js'
import {data as value} from utiliy

console.log(add(2,3))
console.log(value); 


// or you can import all as a bundle which is an object of values.
import * as bundled from utiliy

console.log(bundled.add(2,3));
console.log(bundled.data);


