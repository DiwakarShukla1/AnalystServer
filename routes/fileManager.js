/*var path = require('path'),
	fs = require('fs'),
	q= require('q');

exports.render_data = function(filename) {
	//var file = './rfiles/rdata/'+filename+'.json';
	var file = './rfiles/'+filename+'.json';

	var defered = q.defer();

	path.exists(file, function(exists) { 
	  if (exists) {

	    fs.readFile(file, 'utf8', function (err,json) {
	    if(err) console.log(err)
		  defered.resolve(json);
		});

	  }
	  else defered.reject({status:false});
	});
	return defered.promise;
}
*/