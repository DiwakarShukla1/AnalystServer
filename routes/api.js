var rio = require("rio"),
fs = require('fs'),
fileManager = require('./fileManager');


exports.middlewareAdvice = function(req, res) {
	// res.writeHead(200,{"Content-Type":"application/json"});
	res.header('Access-Control-Allow-Origin','*');
	var method = req.params.methodname;
	var firstparam="";
	console.log("Method Called")
	if(req.params.firstparam) {
		firstparam = req.params.firstparam;
	}

		rio.sourceAndEval("./rfiles/OverallPkg.R", {
		    entryPoint: method,
		    data : {first: firstparam},
		    callback: sendJSON
		});
	

	function sendJSON(err, result) {
		if(!err) {
			console.log('Data returned for : '+req.params.methodname)
			res.send(result)
		}
	
	/*method = method.toLowerCase();
	var promise = fileManager.render_data(method);

    promise.then(function(result) {

        response.send(result);

    }, function(err) {

        if(err.hasOwnProperty('status') && j.status == false) {
            response.send({"err":"No json file found"});
        }
        
    });*/   
}
}


