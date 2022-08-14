// Tell React where to render components (<App/>).
import React from "react";
import ReactDOM from "react-dom/client";

import "./index.css";
import App from "./App"; //app function from the App.js which return JSX code. this is a component

const root = ReactDOM.createRoot(document.getElementById("root")); //create root object
root.render(<App />); // call the render method on it, app
