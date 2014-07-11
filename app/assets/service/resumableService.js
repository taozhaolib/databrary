"use strict";
/*globals Resumable*/
module.factory('resumableService', [
	'$rootScope',
	function ($rootScope){
		var resumableS = {};
		resumableS.makeResumable = function(conf) {return new Resumable(conf);};
		return resumableS;
	}
]);

