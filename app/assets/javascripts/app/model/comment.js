define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Comment', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('comment', '/api/comment/:id', {
			id: '@id'
		}, []);
	}]);
});
