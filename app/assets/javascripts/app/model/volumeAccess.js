define(['app/config/module'], function (module) {
	'use strict';

	module.factory('VolumeAccess', ['$rootScope', 'ResourceService', function ($rootScope, resourceService) {
		return resourceService('volume', '/api/volume/:volumeId/access', {
			volumeId: '@volumeId',
			id: '@id'
		}, []);
	}]);
});
