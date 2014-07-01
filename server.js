// set up ======================================================================
var express  = require('express'),
	fs = require('fs'),
	app = module.exports = express(), 								// create our app w/ express
	server = require('http').createServer(app),
	routes = require('./routes'),
	api = require('./routes/api'),
	fileManager = require('./routes/fileManager');


	app.use(express.static(__dirname + '/public'));
	app.use(express.static(__dirname + '/rfiles'));
	//app.use(express.static(__dirname + '/data'));


// routes ===========================

app.get('/', routes.index);  
app.get('/api/:methodname', api.middlewareAdvice);
app.get('/api/:methodname/:firstparam', api.middlewareAdvice);
//app.get('/api/:fname', fileManager.render_data);




// listen (start app with node server.js) 
var port = Number(process.env.PORT || 8080);
app.listen(port, function() {
  console.log("Listening on " + port);
});