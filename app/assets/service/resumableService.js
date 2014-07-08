module.factory('resumableService', [
	'$rootScope',
	'typeService',
	function ($rootScope, typeService){
		var resumableS = {};
		resumableS.hello = 'hello';
		return resumableS;
	}
]);

