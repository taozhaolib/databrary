module.controller('VolumeEditView', [
	'$scope', 'volume', 'slot', 'pageService', function ($scope, volume, slot, page) {
		page.display.title = page.constants.message('page.title.stub');

		if (volume) {
			page.display.toolbarLinks = [
				{
					type: 'yellow',
					html: page.constants.message('volume.edit.exit'),
					url: page.router.volume({id: volume.id}),
				},
			];
		} else {
			page.display.toolbarLinks = [
				{},
			];
		}

		$scope.volume = volume;

		$scope.funding = [];
		$scope.granted = [];

		if (volume && volume.access) {
			angular.forEach(volume.access, function (access) {
				if (access.hasOwnProperty('funding')) {
					$scope.funding.push(access);
				}

				if (access.access) {
					$scope.granted.push(access);
				}
			});
		}

		//

		var updateQuery = false;

		$scope.$watch(function () {
			return page.$location.search().page;
		}, function (val, old) {
			if (!updateQuery) {
				updateQuery = true;
			}

			if (val && val !== old) {
				for (var step in $scope.wizard.steps) {
					if ($scope.wizard.steps.hasOwnProperty(step) && $scope.wizard.steps[step].id.indexOf(val) > -1) {
						$scope.wizard.activateStep($scope.wizard.steps[step]);
						break;
					}
				}
			}
		});

		//

		$scope.wizard = {};

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.activateFn = activateFn;
		};

		var activateFn = function (step) {
			if (updateQuery) {
				page.$location.search('page', step.id.split('_').pop());
			}
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

				if (angular.isFunction($scope.prepareStep[$scope.wizard.newStep.id])) {
					$scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
				}
			}

			angular.forEach($scope.wizard.steps, function (step) {
				if (angular.isFunction($scope.updateStep[step.id])) {
					$scope.updateStep[step.id](step);
				}
			});
		};

		//

		var forms = {
			overview: undefined,
			citations: undefined,
			materials: undefined,
			funding: undefined,
			access: undefined,
		};

		$scope.$watch(function () {
			for (var form in forms) {
				if (forms.hasOwnProperty(form) && forms[form] && forms[form].$dirty) {
					return forms[form];
				}
			}

			return false;
		}, function (form) {
			angular.forEach($scope.wizard.steps, function (step) {
				if (form && !step.active) {
					step.allow = false;
				} else if (volume) {
					step.allow = true;
				}
			});
		});

		//

		var cancelFn = function () {
			page.$location.url(page.router.volume({id: volume.id}));
		};

		$scope.prepareStep = {
			'volume_edit_overview': function (step) {
				forms.overview = step.volumeEditOverviewForm;
				forms.overview.volume = volume;
				forms.overview.cancelFn = cancelFn;
			},

			'volume_edit_citations': function (step) {
				forms.citations = step.volumeEditCitationsForm;
				forms.citations.volume = volume;
				forms.citations.cancelFn = cancelFn;
			},

			'volume_edit_materials': function (step) {
				forms.materials = step.volumeEditMaterialsForm;
				forms.materials.volume = volume;
				forms.materials.slot = slot;
				forms.materials.cancelFn = cancelFn;
			},

			'volume_edit_funding': function (step) {
				step.enable = page.auth.hasAccess('ADMIN', volume);

				forms.funding = step.volumeEditFundingForm;
				forms.funding.volume = volume;
				forms.funding.cancelFn = cancelFn;
			},

			'volume_edit_access': function (step) {
				step.enable = page.auth.hasAccess('ADMIN', volume);

				forms.access = step.volumeEditAccessForm;
				forms.access.volume = volume;
				forms.access.cancelFn = cancelFn;
			},
		};

		//

		$scope.updateStep = {
			'volume_edit_overview': function (step) {
				if (volume) {
					forms.overview.init({
						name: volume.name,
						alias: volume.alias,
						body: volume.body,
					});
				}
			},

			'volume_edit_citations': function (step) {
				var study, citations = [];

				if (volume) {
					angular.forEach(volume.citations, function (citation) {
						if (citation.study) {
							study = citation;
						} else {
							citations.push(citation);
						}
					});

					forms.citations.init({
						study: study,
						citation: citations,
					});
				}
			},

			'volume_edit_materials': function (step) {
				if (slot) {
					angular.forEach(slot.assets, function (asset) {
						asset.name = asset.asset.name;
						asset.classification = asset.asset.classification;
					});

					forms.materials.init(slot);
				}
			},

			'volume_edit_funding': function (step) {
				forms.funding.data = {
					access: $scope.funding,
				};
			},

			'volume_edit_access': function (step) {
				forms.access.data = {
					access: $scope.granted,
				};
			},
		};
	}
]);
