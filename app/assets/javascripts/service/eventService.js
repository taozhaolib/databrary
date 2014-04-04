define(['config/module'], function (module) {
	'use strict';

	module.factory('eventService', ['$rootScope', function ($rootScope) {
		var events = {};

		//

		events.talk = function (eventName) {
			return $rootScope.$emit.apply($rootScope, arguments);
		};

		events.listen = function ($scope, eventName, callback) {
			var listener = $rootScope.$on(eventName, callback);

			$scope.$on('$destroy', function () {
				listener();
			});

			return listener;
		};

		//

		return events;
	}]);
});
