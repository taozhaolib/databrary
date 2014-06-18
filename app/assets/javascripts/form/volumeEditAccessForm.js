module.directive('volumeEditAccessForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditAccessForm;

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
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				page.models.VolumeAccess.save(form.data,
					function (res) {
						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.access.success'),
						});

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.$setPristine();
						page.models.Volume.$cache.removeAll();
					}, function (res) {
						form.validator.server(res);

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}
					});
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form);
				}

				form.data = $.extend(true, {}, backup);
				form.$setPristine();
			};

			//

			page.events.talk('volumeEditAccessForm-init', form, $scope);

			//

			$scope.$on('accessGrantForm-init', function (event, searchForm) {
				searchForm.successFn = function (searchForm) {
					form.messages.add({
						body: page.constants.message('access.grant.access.save.success'),
						type: 'green',
						countdown: 3000,
					});

					form.$setPristine();
				};

				searchForm.removeSuccessFn = function (searchForm, args, access) {
					form.messages.add({
						body: page.constants.message('access.grant.access.remove.success'),
						type: 'green',
						countdown: 3000,
					});

					form.data.access.splice(form.data.access.indexOf(access), 1);
					form.$setPristine();
				};

				event.stopPropagation();
			});

			$scope.$on('accessSearchForm-init', function (event, searchForm) {
				searchForm.selectFn = function (found) {
					form.data.access.push({
						party: found,
						access: 0,
						inherit: 0,
					});
					form.$setPristine();
				};

				event.stopPropagation();
			});
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditAccessForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
