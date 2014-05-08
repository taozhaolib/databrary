module.directive('volumeEditOverviewForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditOverviewForm;

			form.data = {};
			var backup = {};

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;
			form.resetFn = undefined;
			form.cancelFn = undefined;

			//

			form.init = function (data, volume) {
				form.data = data;
				form.volume = form.volume || volume;
				backup = $.extend(true, {}, data);
			};

			//

			form.save = function () {
				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				if (form.volume) {
					page.models.Volume.save(form.data,
						function (res) {
							page.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.overview.success'),
							});

							if (angular.isFunction(form.successFn))
								form.successFn(form, res);

							form.$setPristine();
						}, function (res) {
							page.messages.addError({
								body: page.constants.message('volume.edit.overview.error'),
								report: res
							});

							if (angular.isFunction(form.errorFn))
								form.errorFn(form, res);
						});
				} else {
					var volume = new page.models.Volume(form.data);

					volume.$save({
						owner: page.auth.user.id
					}, function (res) {
							page.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.overview.success'),
							});

							if (angular.isFunction(form.successFn))
								form.successFn(form, res);

							page.$location.url(page.router.volumeEdit(res));
						}, function (res) {
							page.messages.addError({
								body: page.constants.message('volume.edit.overview.error'),
								report: res
							});

							if (angular.isFunction(form.errorFn))
								form.errorFn(form, res);
						});
				}
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn))
					form.resetFn(form);

				form.data = $.extend(true, {}, backup);
				form.$setPristine();
			};

			form.cancel = function () {
				if (angular.isFunction(form.cancelFn))
					form.cancelFn(form);
			};

			//

			page.events.talk('volumeEditOverviewForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditOverviewForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
