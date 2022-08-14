// one component: component is just a function. Name of component function should start with capital letter, so that when used as an element it is differentiated from the defulat html elements.
// Custom components start with capital letters.
import { useState } from "react"; //State is important for changing events on the screen.
import Backdrop from "./Backdrop";
import Modal from "./Modal";

function Todo(props) {
  // Set the current state
  // Use state returns two elements. The current state value and a function that allows you to change the current state.
  const [modalIsOpen, setModalIsOpen] = useState(false); //useState is a react hook used in a react component funtcion
  function deleteHandler() {
    setModalIsOpen(true);
  }
  function closeModalHander() {
    setModalIsOpen(false);
  }
  return (
    <div className="card">
      <h2>{props.text}</h2>{" "}
      {/*We a place single JS expressions within curly braces. To get the value of the attribute passed object.key */}
      <div className="actions">
        <button className="btn" onClick={deleteHandler}>
          Delete
        </button>
        {/*onClick eventListener, which is handled by the deleteHandler, Remeber no paranthesis as this will make it execute when rendering the elements. */}
      </div>
      {/* We want to render modal and backdrop if modalIsOpen is true */}
      {modalIsOpen && (
        <Modal onCancel={closeModalHander} onConfirm={closeModalHander} />
      )}
      {/*If both expressions are true the las expression is returned*/}
      {modalIsOpen && <Backdrop onCliick={closeModalHander} />}
      {/* Backdrop is userdefined component which don't have builtin eventhandler props, so we have to pass this function closeModalHander along to the component code where we have react buitin component which have event onclick handler props. */}
    </div>
  );
}

export default Todo; // We export to make this function available to other files
