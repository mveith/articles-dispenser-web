import './main.css';
import { Main } from './Main.elm';

var storedLoginData = localStorage.getItem('login-data');
var loginData = storedLoginData ? JSON.parse(storedLoginData) : null;
var app = Main.embed(document.getElementById('root'), loginData);

app.ports.saveLoginData.subscribe(function(data) {
    localStorage.setItem('login-data', JSON.stringify(data));
});