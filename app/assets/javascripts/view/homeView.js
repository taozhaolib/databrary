module.controller('HomeView', [
	'$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
		page.title = page.constants.message('page.title.welcome');
		$scope.parties = parties;
		$scope.volume = volume;
	}
]);
