module.controller('RegisterView', [
	'$scope', 'pageService', function ($scope, page) {
		page.title = page.constants.message('page.title.register');
	}
]);
