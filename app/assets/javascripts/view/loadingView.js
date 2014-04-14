module.controller('LoadingView', ['$scope', 'authPromise', 'constantPromise', 'authService', 'pageService', '$location', function ($scope, authPromise, constantPromise, auth, page, $location) {
	page.loading = true;
	page.title = page.constants.message('page.title.loading');

	if (auth.next) {
		$location.url(auth.next).replace();
		auth.next = undefined;
	}
}]);
