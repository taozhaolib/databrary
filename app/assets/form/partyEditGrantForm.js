'use strict';

module.directive('partyEditGrantForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.partyEditGrantForm;

			form.data = [];

			//

			form.init = function (party, children) {
				form.party = form.party || party;
				form.data = page.$filter('toArray')(children);
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
						subform.save(false);
					}
				});
			};

			form.resetAll = function () {
				angular.forEach(subforms, function (subform) {
					if (subform.$dirty) {
						subform.reset();
					}
				});
			};

			form.scrollToFuture = function (party) {
				var remove = $scope.$watch(function () {
					return subforms[subforms.length - 1];
				}, function (subform) {
					if (subform && subform.other && subform.other.party == party) {
						page.display.scrollTo(subform.$element);
						remove();
					}
				});
			};

			//

			page.events.listen($scope, 'authGrantForm-init', function (event, grantForm) {
				subforms.push(grantForm);

				grantForm.successFn = function () {
					form.messages.add({
						body: page.constants.message('auth.grant.save.success'),
						type: 'green',
						countdown: 3000,
					});
				};

				grantForm.denySuccessFn = function (grantForm) {
					form.messages.add({
						body: page.constants.message('auth.grant.remove.success'),
						type: 'green',
						countdown: 3000,
					});

					form.data.splice(form.data.indexOf(grantForm.other), 1);
					subforms.splice(subforms.indexOf(grantForm), 1);
				};

				event.stopPropagation();
			});

			page.events.listen($scope, 'authSearchForm-init', function (event, searchForm) {
				if (searchForm.principal != 'child') {
					return;
				}

				searchForm.selectFn = function (found) {
					var present = false;

					angular.forEach(form.data, function (access, i) {
						if (access.party.id === found.id) {
							var el = form.data.splice(i, 1)[0];
							form.data.push(el);
							present = true;

							searchForm.messages.add({
								type: 'yellow',
								countdown: 3000,
								body: page.constants.message('access.search.repeat', found.name),
							});

							return false;
						}
					});

					if (!present) {
						form.data.push({
							new: true,
							party: found,
							site: 0,
							member: 0,
						});
					}
				};

				searchForm.notFoundFn = function () {
					page.messages.add({
						type: 'yellow',
						countdown: 3000,
						body: page.constants.message('auth.grant.notfound.message')
					});
				};

				event.stopPropagation();
			});

			//

			page.events.talk('partyEditGrantForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'partyEditGrantForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
