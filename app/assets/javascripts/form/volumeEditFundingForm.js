module.directive('volumeEditFundingForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditFundingForm;

			form.data = {};
			form.volume = undefined;
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

				page.models.VolumeAccess.save(form.data,
					function (res) {
						page.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.funding.success'),
						});

						if (angular.isFunction(form.successFn))
							form.successFn(form, res);

						form.$setPristine();
						page.models.Volume.$cache.removeAll();
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('volume.edit.funding.error'),
							report: res
						});

						if (angular.isFunction(form.errorFn))
							form.errorFn(form, res);
					});
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

			page.events.talk('volumeEditFundingForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditFundingForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
