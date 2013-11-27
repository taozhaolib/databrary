define(['app/modules/dbServices'], function (db) {
	'use strict';

	db.factory('EventService', ['$rootScope', function ($rootScope) {
		var eventService = {};

		eventService.broadcast = function (name) {
			$rootScope.$broadcast.apply($rootScope, arguments);
		};

		return eventService;
	}]);
});
