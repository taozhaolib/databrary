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

			var subforms = [];

			$scope.$watch(function () {
				var clean = true;

				angular.forEach(subforms, function (subform) {
					if (subform.$dirty) {
						clean = false;
						return false;
					}
				});

				if (clean) {
					form.$setPristine();
				}
			});

			form.saveAll = function () {
				angular.forEach(subforms, function (subform) {
					if (subform.$dirty) {
						subform.save();
					}
				});
			};

			form.resetAll = function () {
				angular.forEach(subforms, function (subform, id) {
					if (subform.$dirty) {
						subform.reset();
					}
				});
			};

			//

			page.events.talk('volumeEditAccessForm-init', form, $scope);

			//

			$scope.$on('accessGrantForm-init', function (event, grantForm) {
				subforms.push(grantForm);

				grantForm.successFn = function (grantForm) {
					form.messages.add({
						body: page.constants.message('access.grant.access.save.success'),
						type: 'green',
						countdown: 3000,
					});
				};

				grantForm.removeSuccessFn = function (grantForm, args, access) {
					form.messages.add({
						body: page.constants.message('access.grant.access.remove.success'),
						type: 'green',
						countdown: 3000,
					});

					form.data.access.splice(form.data.access.indexOf(access), 1);
				};

				event.stopPropagation();
			});

			$scope.$on('accessSearchForm-init', function (event, searchForm) {
				searchForm.selectFn = function (found) {
					var present = false;

					angular.forEach(form.data.access, function (access, i) {
						if (access.party.id === found.id) {
							var el = form.data.access.splice(i, 1)[0];
							form.data.access.push(el);
							present = true;
							return false;
						}
					});

					if (!present) {
						form.data.access.push({
							party: found,
							access: 0,
							inherit: 0,
						});
					} else {
						searchForm.messages.add({
							type: 'yellow',
							countdown: 3000,
							body: page.constants.message('access.search.repeat', found.name),
						});
					}
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
