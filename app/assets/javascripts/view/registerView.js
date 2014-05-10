module.controller('RegisterView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.register');
	}
]);
