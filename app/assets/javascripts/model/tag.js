define(['config/module'], function (module) {
	'use strict';

	module.factory('Tag', ['$resource', function ($resource) {
		return $resource('/api/tag/:id', {});
	}]);
});
