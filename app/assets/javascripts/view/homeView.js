module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.display.title = page.constants.message('page.title.home');

		$scope.parties = parties;
		$scope.volume = volume;
	}
]);
