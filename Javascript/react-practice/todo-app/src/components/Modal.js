// contains the delete modal
function Modal(props) {
  return (
    <div className="modal">
      <p>Are you sure you want to delete ?</p>
      {/* Each button has to have seperate event listeners */}
      <button className="btn" onClick={props.onConfirm}>
        Confirm
      </button>
      <button className="btn btn--" onClick={props.onCancel}>
        Cancel
      </button>
    </div>
  );
}

export default Modal;
