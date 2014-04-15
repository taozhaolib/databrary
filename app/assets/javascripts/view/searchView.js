module.controller('SearchView', [
	'$scope', 'volumes', 'pageService', function ($scope, volumes, page) {
		$scope.volumes = volumes;
		page.title = page.constants.message('page.title.search');

		angular.forEach($scope.volumes, function (volume) {
			volume.more = '';

			angular.forEach(volume.access, function (access) {
				volume.more += ' ' + access.party.name + ' ' + access.party.email +
					(access.party.affiliation ? ' ' + access.party.affiliation : '');
			});
		});

		$scope.browser.initialize('search', volumes);
	}
]);
