'use strict';

module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.display.title = page.constants.message('page.title.home');

		var lastName = function (p) {
			var s = p.name;
			if (!s) {
				return '';
			}
			return s.substr(s.lastIndexOf(' '));
		};

		$scope.parties = page.$filter('toArray')(parties, lastName);
		$scope.volume = volume;

	}
]);
