define(['app/config/module'], function (module) {
	'use strict';

	module.factory('SlotAsset', ['$rootScope', 'ResourceService', function ($rootScope, resourceService) {
		return resourceService('slotAsset', '/api/slot/:slotId/asset/:id', {
			slotId: '@slotId',
			id: '@id'
		}, []);
	}]);
});
