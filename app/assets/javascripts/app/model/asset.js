define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Asset', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('asset', '/api/asset/:id', {
			id: '@id'
		}, []);
	}]);
});
