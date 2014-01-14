define(['app/config/module'], function (module) {
	'use strict';

	module.factory('SlotAsset', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/slot/:slotId/asset/:id', {
			slotId: function () {
				return $route.current.params.slotId || false;
			},
			id: function () {
				return $route.current.params.id || false;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		});
	}]);
});
