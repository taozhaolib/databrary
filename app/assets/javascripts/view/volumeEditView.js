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
				{
					type: 'yellow',
					html: page.constants.message('volume.edit.cancel'),
					url: page.router.prevUrl(),
				}
			];
		}

		$scope.volume = volume;

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

		// TODO: SEND THIS INFO FROM THE START

		if (slot) {
			angular.forEach(slot.assets, function (asset) {
				page.models.Asset.get({
					creation: '',
					id: asset.asset.id
				}, function (res) {
					asset.asset.creation = res.creation;
				});
			});
		}

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
			excerpts: undefined,
			materials: undefined,
			funding: undefined,
			access: undefined,
		};

		page.display.navigationFn = function (event, val) {
			if (!volume || val.indexOf('/volume/'+volume.id+'/edit') > -1) {
				return;
			}

			for (var id in forms) {
				if (forms.hasOwnProperty(id) && forms[id] && forms[id].form && forms[id].form.$dirty) {
					return false;
				}
			}

			return true;
		};

		$scope.$watch(function () {
			angular.forEach(forms, function (form) {
				if (!form || !form.form) {
					return;
				}

				if (form.form.$invalid) {
					form.step.complete = false;
				} else if (form.form.$dirty) {
					form.step.complete = undefined;
				} else if (form.step.allow) {
					form.step.complete = true;
				}
			});
		});

		//

		var cancelFn = function () {
			page.$location.url(page.router.volume({id: volume.id}));
		};

		$scope.prepareStep = {
			'volume_edit_overview': function (step) {
				forms.overview = {
					step: step,
					form: step.volumeEditOverviewForm,
				};
				forms.overview.form.volume = volume;
				forms.overview.form.cancelFn = cancelFn;
			},

			'volume_edit_excerpts': function (step) {
				if(!volume)
					return;

				forms.excerpts = {
					step: step,
					form: step.volumeEditMaterialsForm,
				};
				forms.excerpts.form.volume = volume;
				forms.excerpts.form.slot = slot;
				forms.excerpts.form.cancelFn = cancelFn;
			},

			'volume_edit_materials': function (step) {
				if(!volume)
					return;

				forms.materials = {
					step: step,
					form: step.volumeEditMaterialsForm,
				};
				forms.materials.form.volume = volume;
				forms.materials.form.slot = slot;
				forms.materials.form.cancelFn = cancelFn;
			},

			'volume_edit_funding': function (step) {
				step.enable = page.auth.hasAccess('ADMIN', volume);

				if(!volume)
					return;

				forms.funding = {
					step: step,
					form: step.volumeEditFundingForm,
				};
				forms.funding.form.init(volume.funding, volume);
				forms.funding.form.cancelFn = cancelFn;
			},

			'volume_edit_access': function (step) {
				step.enable = page.auth.hasAccess('ADMIN', volume);

				if(!volume)
					return;

				forms.access = {
					step: step,
					form: step.volumeEditAccessForm,
				};
				forms.access.form.volume = volume;
				forms.access.form.cancelFn = cancelFn;
			},
		};

		//

		$scope.updateStep = {
			'volume_edit_overview': function (step) {
				if (volume) {
					forms.overview.form.init({
						name: volume.name,
						alias: volume.alias,
						body: volume.body,
						citation: volume.citation,
					}, volume);
				} else {
					forms.overview.form.init({}, volume);
				}
			},

			'volume_edit_citations': function (step) {
				if(!volume)
					return;

				var study, citations = [];

				if (volume) {
					angular.forEach(volume.citations, function (citation) {
						if (citation.study) {
							study = citation;
						} else {
							citations.push(citation);
						}
					});

					forms.citations.form.init({
						study: study,
						citation: citations,
					});
				}
			},

			'volume_edit_excerpts': function (step) {
				if(!volume)
					return;

				if (slot) {
					angular.forEach(slot.assets, function (asset) {
						asset.name = asset.asset.name;
						asset.classification = '' + asset.asset.classification;
					});

					forms.excerpts.form.init(slot);
				}
			},

			'volume_edit_materials': function (step) {
				if(!volume)
					return;

				if (slot) {
					angular.forEach(slot.assets, function (asset) {
						asset.name = asset.asset.name;
						asset.classification = '' + asset.asset.classification;
					});

					forms.materials.form.init(slot);
				}
			},

			'volume_edit_funding': function (step) {
				if(!volume)
					return;

				forms.funding.form.init(volume.funding, volume);
			},

			'volume_edit_access': function (step) {
				if(!volume)
					return;

				forms.access.form.data = {
					access: volume.access,
				};
			},
		};
	}
]);
