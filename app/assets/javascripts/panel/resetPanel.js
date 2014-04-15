module.controller('ResetPanel', [
	'$scope',
	'authService',
	'pageService',
	'$location',
	function ($scope, authService, page, $location) {
		page.events.listen($scope, 'userPasswordForm-init', function (event, form) {
			form.resetSuccessFn = function () {
				$location.url(page.router.index());
			};

			form.saveSuccessFn = function () {
				$location.url(page.router.index());
			};

			event.stopPropagation();
		});
	}
]);
