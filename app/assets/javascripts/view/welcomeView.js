module.controller('WelcomeView', ['$scope', 'pageService', function ($scope, page) {
	page.title = page.constants.message('page.title.welcome');
}]);
