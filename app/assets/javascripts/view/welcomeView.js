module.controller('WelcomeView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.welcome');
	}
]);
