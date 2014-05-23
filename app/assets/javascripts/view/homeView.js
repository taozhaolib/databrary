module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.display.title = page.constants.message('page.title.welcome');
		$scope.parties = parties;
		$scope.volume = volume;

		$scope.columns = [];

		var $wrap = $('.wrap').first();

		var resize = function (width) {
			var cols = 1;
			width = parseInt(width);
			$scope.columns = [];

			if (width >= 1200) {
				cols = 5;
			} else if (width >= 960) {
				cols = 4;
			} else if (width >= 720) {
				cols = 3;
			} else if (width >= 480) {
				cols = 2;
			}

			var col, i = 0;

			angular.forEach(parties, function (party) {
				if (!party.id) {
					return;
				}

				col = i++ % cols;

				if (!$scope.columns[col]) {
					$scope.columns[col] = [];
				}

				$scope.columns[col].push(party);
			});
		};

		$scope.$watch(function () {
			return $wrap.outerWidth();
		}, function (width) {
			resize(width);
		});

		page.$w.on('resize.homeView', function () {
			$scope.$apply(function () {
				resize($wrap.outerWidth());
			});
		});

		$scope.$on('$destroy', function () {
			page.$w.off('resize.homeView');
		});
	}
]);
