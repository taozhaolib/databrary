define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Tag', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('tag', '/api/tag/:id', {
			id: '@id'
		}, []);
	}]);
});
