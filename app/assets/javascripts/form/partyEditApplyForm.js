module.directive('partyEditApplyForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.partyEditApplyForm;

			form.data = {};

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
				angular.forEach(subforms, function (subform, id) {
					if (subform.$dirty) {
						subform.cancel();
					}
				});
			};

			//

			form.presetName = function (type, name, party) {
				if (angular.isString(party)) {
					return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.constants.message('auth.' + type + '.' + name, party);
				} else {
					return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
				}
			};

			//

			page.events.listen($scope, 'authApplyForm-init', function (event, grantForm) {
				subforms.push(grantForm);

				grantForm.successFn = function (grantForm) {
					form.messages.add({
						body: page.constants.message('auth.apply.save.success'),
						type: 'green',
						countdown: 3000,
					});
				};

				grantForm.cancelFn = function (grantForm, args, access) {
					form.messages.add({
						body: page.constants.message('auth.apply.remove.success'),
						type: 'green',
						countdown: 3000,
					});

					form.data.splice(form.data.indexOf(access), 1);
					subforms.splice(subforms.indexOf(grantForm), 1);
				};

				event.stopPropagation();
			});

			page.events.listen($scope, 'authSearchForm-init', function (event, searchForm) {
				if (searchForm.principal == 'child') {
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

				event.stopPropagation();
			});

			//

			page.events.talk('partyEditApplyForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'partyEditApplyForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
