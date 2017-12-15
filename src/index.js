import './main.css';
import { Main } from './Main.elm';

var storedData = localStorage.getItem('model');
var data = storedData ? JSON.parse(storedData) : null;
var app = Main.embed(document.getElementById('root'), data);

app.ports.saveData.subscribe(function(data) {
    localStorage.setItem('model', JSON.stringify(data));
});