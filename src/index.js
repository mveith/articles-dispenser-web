import './main.css';
import { Elm } from './Main.elm';

var storedData = localStorage.getItem('model');
var data = storedData ? JSON.parse(storedData) : null;
var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: data
});
app.ports.saveData.subscribe(function(data) {
  localStorage.setItem('model', JSON.stringify(data));
});