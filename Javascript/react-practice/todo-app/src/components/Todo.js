// contains the todo card.
import { useState } from "react";
import Modal from "./Modal";
import Backdrop from "./Backdrop";

function Todo(props) {
  const [modalprompt, setmodalprompt] = useState(false);
  function deleteHandler(params) {
    setmodalprompt(true);
  }

  function closeModalHandler() {
    setmodalprompt(false);
  }

  function confirmModalHandler() {
    setmodalprompt(false);
  }

  return (
    <div className="card">
      <h2>Title</h2>
      <button className="btn">add</button>
      <button className="btn btn--alt" onClick={deleteHandler}>
        delete
      </button>
      {modalprompt && (
        <Modal onCancel={closeModalHandler} onConfirm={confirmModalHandler} />
      )}
      {modalprompt && <Backdrop onClick={closeModalHandler} />}
    </div>
  );
}

export default Todo;
