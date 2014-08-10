module.controller('SearchView', [
	'$scope', 'volumes', 'pageService', function ($scope, volumes, page) {
		page.display.title = page.constants.message('page.title.search');

		//

		var updateData = function (data) {
			angular.forEach(data, function (volume) {
				volume.more = '';

				angular.forEach(volume.access, function (access) {
				    if (access.individual >= page.permission.ADMIN) {
					volume.more += ' ' + access.party.name;
					if ('email' in access.party)
					    volume.more += ' ' + access.party.email;
					if ('affiliation' in access.party)
					    volume.more += ' ' + access.party.affiliation;
				    }
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
