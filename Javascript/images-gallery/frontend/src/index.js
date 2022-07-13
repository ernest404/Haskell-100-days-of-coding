import React from 'react'; //importing React from react package
import ReactDOM from 'react-dom/client'; // /importing ReactDOM from react-dom/client package
import './css/index.css'; // ./ is the current folder
import App from './App';

const root = ReactDOM.createRoot(document.getElementById('root')); //the point of insertion for our react app
root.render(
  <React.StrictMode>
    <App />
    <h1>Hello from react</h1>
  </React.StrictMode>
);

