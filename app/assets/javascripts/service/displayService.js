define(['config/module'], function (module) {
	'use strict';

	module.factory('DisplayService', ['$sessionStorage', 'EventService', '$filter', function ($sessionStorage, events, $filter) {
		var display = {};

		//

		var ageKeys = ['science', 'days', 'months', 'years'],
			ageKey = $sessionStorage['displayAge'] || 'science';

		display.toggleAge = function () {
			ageKey = ageKeys[(ageKeys.indexOf(ageKey) + 1) % ageKeys.length];
			events.talk('DisplayService-toggleAge', ageKey);
			$sessionStorage['displayAge'] = ageKey;
		};

		display.formatAge = function (value) {
			return $filter('age')(value, ageKey);
		};

		//

		return display;
	}]);
});
