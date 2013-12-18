define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Slot', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('slot', '/api/slot/:id', {
			id: '@id'
		}, [
			'assets',
			'records',
			'tags',
			'comments'
		]);
	}]);
});
