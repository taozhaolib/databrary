module.controller('searchView', [
	'$scope', 'volumes', 'pageService', function ($scope, volumes, page) {
		page.display.title = page.constants.message('page.title.search');

		//

		var updateData = function (data) {
			angular.forEach(data, function (volume) {
				volume.more = '';

				angular.forEach(volume.access, function (access) {
					volume.more += ' ' + access.party.name + ' ' + access.party.email +
						(access.party.affiliation ? ' ' + access.party.affiliation : '');
				});
			});

			$scope.volumes = data;
		};

		updateData(volumes);

		//

		page.events.listen($scope, 'searchForm-init', function (form) {
			$scope.searchForm = $scope.searchForm || form;
		});
	}
]);
