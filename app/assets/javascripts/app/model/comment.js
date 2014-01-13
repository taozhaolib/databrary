define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Comment', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/comment/:id', {
			id: '@id'
		});
	}]);
});
