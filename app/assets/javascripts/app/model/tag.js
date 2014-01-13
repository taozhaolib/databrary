define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Tag', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/tag/:id', {
			id: '@id'
		});
	}]);
});
