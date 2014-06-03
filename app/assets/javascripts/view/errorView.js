module.controller('ErrorView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.error');

		if (!page.display.error) {
			page.$location.url(page.router.index());
		}

		$scope.error = page.display.error;
		page.display.error = undefined;
	}
]);
