"use strict";
/*globals Resumable*/
>>>>>>> Stashed changes
module.factory('resumableService', [
	'$rootScope',
	'typeService',
	function ($rootScope, typeService){
		var resumableS = {};
		resumableS.makeResumable = function(conf) {return new Resumable(conf);};
		return resumableS;
	}
]);

