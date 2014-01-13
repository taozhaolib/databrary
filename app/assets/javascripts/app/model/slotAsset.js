define(['app/config/module'], function (module) {
	'use strict';

	module.factory('SlotAsset', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/slot/:slotId/asset/:id', {
			slotId: '@slotId',
			id: '@id'
		});
	}]);
});
