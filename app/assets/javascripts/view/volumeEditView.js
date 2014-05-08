module.controller('VolumeEditView', [
	'$scope', 'volume', 'slot', 'pageService', function ($scope, volume, slot, page) {
		page.title = page.constants.message('page.title.stub');
		$scope.volume = volume;

		$scope.$watch(function () {
			return page.$location.search().page;
		}, function (val, old) {
			if (val && val !== old) {
				for (var step in $scope.wizard.steps) {
					if ($scope.wizard.steps.hasOwnProperty(step) && $scope.wizard.steps[step].id.indexOf(val) > -1) {
						$scope.wizard.activateStep($scope.wizard.steps[step]);
						break;
					}
				}
			}
		});

		var activateFn = function (step) { console.log(step.id.split('_'));
			page.$location.search('page', step.id.split('_').pop());
		};

		//

		$scope.wizard = {};

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.activateFn = activateFn;
		};

		$scope.updateWizard = function () {
			if ($scope.wizard.newStep) {
				$scope.wizard.newStep.complete = false;
				$scope.wizard.newStep.allow = !!(volume || $scope.wizard.newStep.id === 'volume_edit_overview');

				if (page.$location.search().page && $scope.wizard.newStep.id.indexOf(page.$location.search().page) > -1) {
					$scope.wizard.activateStep($scope.wizard.newStep);
				} else if ($scope.wizard.newStep.id === 'volume_edit_overview') {
					$scope.wizard.activateStep($scope.wizard.newStep);
				}

				if (angular.isFunction($scope.prepareStep[$scope.wizard.newStep.id]))
					$scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
			}

			angular.forEach($scope.wizard.steps, function (step) {
				if (angular.isFunction($scope.updateStep[step.id](step)))
					$scope.updateStep[step.id](step);
			});
		};

		//

		var forms = {
			overview: undefined,
			publications: undefined,
			materials: undefined,
			access: undefined,
		};

		$scope.$watch(function () {
			for (var form in forms) {
				if (forms.hasOwnProperty(form) && forms[form] && forms[form].$dirty)
					return forms[form];
			}

			return false;
		}, function (form) {
			angular.forEach($scope.wizard.steps, function (step) {
				if (form && !step.active) {
					step.allow = false;
				} else {
					step.allow = true;
				}
			});
		});

		//

		var cancelFn = function () {
			console.log(page.router.volume({id: volume.id}));
			page.$location.url(page.router.volume({id: volume.id}));
		};

		$scope.prepareStep = {
			'volume_edit_overview': function (step) {
				forms.overview = step.volumeEditOverviewForm;
				forms.overview.volume = volume;
				forms.overview.cancelFn = cancelFn;
			},

			'volume_edit_publications': function (step) {
				forms.publications = step.volumeEditPublicationsForm;
				forms.publications.volume = volume;
				forms.publications.cancelFn = cancelFn;
			},

			'volume_edit_materials': function (step) {
				forms.materials = step.volumeEditMaterialsForm;
				forms.materials.volume = volume;
				forms.materials.cancelFn = cancelFn;
			},

			'volume_edit_access': function (step) {
				forms.access = step.volumeEditAccessForm;
				forms.access.volume = volume;
				forms.access.cancelFn = cancelFn;
			},
		};

		//

		$scope.updateStep = {
			'volume_edit_overview': function (step) {
				if (volume)
					forms.overview.init({
						name: volume.name,
						alias: volume.alias,
						body: volume.body,
					});
			},

			'volume_edit_publications': function (step) {
				if (volume)
					forms.publications.init({
						citation: volume.citations,
					});
			},

			'volume_edit_materials': function (step) {
				if (slot)
					forms.materials.data = slot;
			},

			'volume_edit_access': function (step) {
				forms.access.data = {};
			},
		};
	}
]);
