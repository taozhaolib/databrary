module.controller('SearchView', [
	'$scope', 'volumes', 'pageService', function ($scope, volumes, page) {
		page.title = page.constants.message('page.title.search');

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

		var successFn = function (form, res) {
			updateData(res);
		};

		page.events.listen($scope, 'searchForm-init', function (form) {
			$scope.searchForm = $scope.searchForm || form;
			$scope.searchForm.successFn = successFn;
		});

		//

		$scope.filterVolumes = function () {
			if ($scope.searchForm.filterMode) {
				return page.$filter('filter')($scope.volumes, $scope.searchForm.data.query);
			}

			return $scope.volumes;
		};

		$scope.volumeClasses = function (volume) {
			var cls = [], study = false;

			if (Object.keys(volume.providers).length === 0) {
				cls.push('dataset');
			} else {
				cls.push('study');
			}

				return cls;
		};

		$scope.iconClass = function (volume) {
			var cls = [];

			if (Object.keys(volume.providers).length === 0) {
				cls.push('dataset');
			} else {
				cls.push('study');
			}

			return cls;
		};

		//

		var tips = {};

		var bindTooltips = function () {
			var newtips = {
				'.search_results .icon.dataset': page.constants.message('object.tip.dataset'),
				'.search_results .icon.study': page.constants.message('object.tip.study')
			};

			angular.forEach(newtips, function (message, target) {
				tips[target] = page.tooltips.add({
					live: true,
					$target: target,
					message: message
				});
			});
		};

		bindTooltips();

		$scope.$on('$destroy', function () {
			angular.forEach(tips, function (tip) {
				page.tooltips.remove(tip);
			})
		});
	}
]);
