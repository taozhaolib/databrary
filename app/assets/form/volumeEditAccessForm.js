'use strict';

module.directive('volumeEditAccessForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditAccessForm;

			form.data = [];
			form.global = angular.copy(page.constants.data.accessGlobal[0]);

			form.volume = undefined;
			var backup = [];

			//

			form.init = function (data, volume) {
				if (form.data.length === 0) {
					angular.forEach(data, function (access) {
						var i = page.constants.data.accessGlobal.parties.indexOf(access.party.id);
						if (i >= 0) {
							form.global[i] = page.constants.data.permission[access.children || 0];
						} else {
							form.data.push(access);
						}
					});
					form.calcGlobalVal();

					form.volume = form.volume || volume;
					angular.copy(form.global, backup);
				}
			};

			form.calcGlobalVal = function () {
				form.globalVal = undefined;

				angular.forEach(page.constants.data.accessGlobal, function (preset, i) {
					if (preset.every(function (x, i) {
						return form.global[i] === x;
					})) {
						form.globalVal = i;
						return false;
					}
				});
			};

			form.changeAccessGlobal = function () {
				angular.copy(page.constants.data.accessGlobal[form.globalVal], form.global);
				form.accessGlobalDirty = true;
				form.$setDirty();
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

			//

			form.saveGlobalFn = undefined;
			form.errorGlobalFn = undefined;
			form.successGlobalFn = undefined;

			form.saveGlobal = function () {
				if (angular.isFunction(form.saveGlobalFn)) {
					form.saveGlobalFn(form);
				}

				page.$q.all(page.constants.data.accessGlobal.parties.map(function (party, i) {
					var p = page.constants.data.permissionName[form.global[i]];
					page.models.volumeAccess.save({
						id: form.volume.id,
						partyId: party
					}, {
						individual: p,
						children: p,
					});
				})).then(function () {
					if (angular.isFunction(form.successGlobalFn)) {
						form.successGlobalFn(form, arguments);
					}

					form.messages.add({
						body: page.constants.message('access.global.save.success'),
						type: 'green',
						countdown: 3000,
					});

					angular.copy(form.global, backup);
					form.accessGlobalDirty = false;
					form.$setPristine();
					page.models.volume.$cache.removeAll();
				}, function (res) {
					form.messages.addError({
						body: page.constants.message('access.global.save.error'),
						report: res,
					});
					page.display.scrollTo(form.$element);

					if (angular.isFunction(form.errorGlobalFn)) {
						form.errorGlobalFn(form, arguments);
					}
				});
			};

			form.resetGlobalFn = undefined;

			form.resetGlobal = function () {
				if (angular.isFunction(form.resetGlobalFn)) {
					form.resetGlobalFn(form);
				}

				angular.copy(backup, form.global);
				form.calcGlobalVal();
				form.accessGlobalDirty = false;
				form.$setPristine();
			};

			//

			page.events.talk('volumeEditAccessForm-init', form, $scope);

			//

			$scope.$on('accessGrantForm-init', function (event, grantForm) {
				subforms.push(grantForm);

				grantForm.successFn = function () {
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

					form.data.splice(form.data.indexOf(access), 1);
				};

				event.stopPropagation();
			});

			$scope.$on('accessSearchForm-init', function (event, searchForm) {
				searchForm.selectFn = function (found) {
					var present = false;

					angular.forEach(form.data, function (access, i) {
						if (access.party.id === found.id) {
							var el = form.data.splice(i, 1)[0];
							form.data.push(el);
							present = true;
							return false;
						}
					});

					if (!present) {
						form.data.push({
							new: true,
							party: found,
							individual: 0,
							children: 0,
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
