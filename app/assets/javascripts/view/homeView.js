module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.display.title = page.constants.message('page.title.welcome');
		$scope.parties = parties;
		$scope.volume = volume;

		$scope.rows = [];

		var $wrap = $('.wrap').first();

		var resize = function (width) {
			var parties_per_row = 1;
			width = parseInt(width);
			$scope.rows = [];

			if (width >= 1200) {
				parties_per_row = 5;
			} else if (width >= 960) {
				parties_per_row = 4;
			} else if (width >= 720) {
				parties_per_row = 3;
			} else if (width >= 480) {
				parties_per_row = 2;
			}

			var row = -1, i = 0;

			angular.forEach(parties, function (party) {
				if (!party.id) {
					return;
				}
				
				if (i % parties_per_row == 0) row++;
				i++;

				if (!$scope.rows[row]) {
					$scope.rows[row] = [];
				}

				$scope.rows[row].push(party);
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
