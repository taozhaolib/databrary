define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Record', ['$rootScope', 'ResourceService', function ($rootScope, resourceService) {
		return resourceService('record', '/api/record/:id', {
			id: '@id'
		}, [
//			'assets',
			'slots'
		]);
	}]);
});
