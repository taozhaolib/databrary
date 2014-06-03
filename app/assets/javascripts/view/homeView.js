module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.display.title = page.constants.message('page.title.home');
		$scope.parties = parties;
		$scope.volume = volume;

		$scope.rows = [];

		var $wrap = $('.wrap').first();

		var resize = function (width) {
			width = parseInt(width);
			$scope.rows = [];

			var PIXELS_PER_PARTY = 240;
			var parties_per_row = 
				Math.max(1, Math.min(5, Math.floor(width/PIXELS_PER_PARTY)));

			var i = 0;
			angular.forEach(parties, function (party) {
				if (!party.id) {
					return;
				}

				var row = Math.floor(i/parties_per_row);
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
