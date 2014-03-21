define(['config/module'], function (module) {
	'use strict';

	module.factory('EventService', ['$rootScope', function ($rootScope) {
		var eventService = {};

		//

		eventService.talk = function (eventName) {
			return $rootScope.$emit.apply($rootScope, arguments);
		};

		eventService.listen = function ($scope, eventName, callback) {
			var listener = $rootScope.$on(eventName, callback);

			$scope.$on('$destroy', function () {
				listener();
			});

			return listener;
		};

		//

		return eventService;
	}]);
});
