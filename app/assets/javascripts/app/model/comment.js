define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Comment', ['$rootScope', 'ResourceService', function ($rootScope, resourceService) {
		return resourceService('comment', '/api/comment/:id', {
			id: '@id'
		}, []);
	}]);
});
