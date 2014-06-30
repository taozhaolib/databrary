module.controller('ResetView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.reset');

		//

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
