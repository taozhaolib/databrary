define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Volume', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('volume', '/api/volume/:id', {
			id: '@id'
		}, [
			'access',
			'citations',
			'tags',
			'comments',
			'categories',
			'funding'
		]);
	}]);
});
