define(['config/module'], function (module) {
	'use strict';

	module.factory('EventService', ['$rootScope', function ($rootScope) {
		var eventService = {};

		//

		var listeners = {};

		var destroyListener = function ($event, $scope) {
			if (typeof($event) != 'undefined')
				$scope = $event.$scope;

			$.each(listeners[$scope.$id].events, function (key) {
				listeners[$scope.$id].events[key];
			});

			listeners[$scope.$id].destroyer;

			delete listeners[$scope.$id];

			return true;
		};

		//

		eventService.talk = function (eventName) {
			return $rootScope.$emit.apply($rootScope, arguments);
		};

		eventService.listen = function ($scope, eventName, callback) {
			var listener = $rootScope.$on(eventName, callback);

			if (!listeners[$scope.$id])
				listeners[$scope.$id] = {
					destroyer: $scope.$on('$destroy', destroyListener),
					events: {}
				};

			listeners[$scope.$id].events[eventName] = listener;

			return listener;
		};

		eventService.destroyListener = function ($scope) {
			return destroyListener(undefined, $scope);
		};

		eventService.destroyEvent = function ($scope, eventName) {
			return listeners[$scope.$id].events[eventName];
		};

		return eventService;
	}]);
});
