define(['config/module'], function (module) {
	'use strict';

	module.controller('SearchView', ['$scope', 'volumes', 'PageService', 'ConstantService', function ($scope, volumes, page, constants) {
		$scope.volumes = volumes;
		page.title = constants.message('page.title.search');

		angular.forEach($scope.volumes, function (volume) {
			volume.more = '';

			angular.forEach(volume.access, function (access) {
				volume.more += ' ' + access.party.name + ' ' + access.party.email +
						(access.party.affiliation ? ' ' + access.party.affiliation : '');
			});
		});

		$scope.browser.initialize('search', volumes);
	}]);
});
