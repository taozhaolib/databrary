define(['app/config/module'], function (module) {
	'use strict';

	module.factory('VolumeAccess', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/volume/:volumeId/access', {
			volumeId: '@volumeId',
			id: '@id'
		});
	}]);
});
