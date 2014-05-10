module.controller('ResetPanel', [
	'$scope', 'pageService', function ($scope, page) {
		page.events.listen($scope, 'userPasswordForm-init', function (event, form) {
			form.resetSuccessFn = function () {
				page.$location.url(page.router.index());
			};

			form.saveSuccessFn = function () {
				page.$location.url(page.router.index());
			};

			event.stopPropagation();
		});
	}
]);
