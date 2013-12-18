define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Volume', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('volume', '/api/volume/:id', {
			id: '@id'
		}, [
			'access',
			'citations',
			'top',
			'tags',
			'comments',
			'categories',
			'records',
			'summary',
			'sessions',
			'assets',
			'funding'
		]);
	}]);
});
