define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Tag', ['$rootScope', 'ResourceService', function ($rootScope, resourceService) {
		return resourceService('tag', '/api/tag/:id', {
			id: '@id'
		}, []);
	}]);
});
