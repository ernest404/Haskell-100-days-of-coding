// Contains the root component where other components are attached to
import Todo from "./components/Todo";

function App() {
  return (
    <div>
      <Todo />
      <Todo />
      <Todo />
    </div>
  );
}

export default App;
