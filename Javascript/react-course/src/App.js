// This is the root component other compents are added to it.

import Todo from "./components/Todo";

function App() {
  return (
    <div>
      <h1>My Todos</h1>
      {/* We use the custom Todo.js component as an element. We can also pass a text parameter to it to make it dynamic. 
      These attributes are passed as key-value pairs object called props  */}
      <Todo text="Learn React" />
      <Todo text="Learn Haskell" />
      <Todo text="Create course" />
    </div>
  );
}

export default App;
