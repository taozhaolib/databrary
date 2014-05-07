module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.title = page.constants.message('page.title.welcome');
		$scope.parties = parties;
		$scope.volume = volume;

		$scope.columns = [];

		var $wrap = $('.wrap').first();

		$scope.$watch(function () {
			return $wrap.css('width');
		}, function (val, old) {
			var cols = 1;
			val = parseInt(val);
			$scope.columns = [];

			if(val >= 1200) {
				cols = 5;
			} else if (val >= 960) {
				cols = 4;
			} else if (val >= 720) {
				cols = 3;
			} else if (val >= 480) {
				cols = 2;
			}

			var col, i = 0;

			angular.forEach(parties, function (party, key) {
				if (!party.id)
					return;

				col = i++ % cols;

				if (!$scope.columns[col])
					$scope.columns[col] = [];

				$scope.columns[col].push(party);
			});
		});
	}
]);
