function Backdrop(props) {
  return <div className="backdrop" onClick={props.onCliick} />; // allowed in JSX when we have content in between the tags.
}

export default Backdrop;
