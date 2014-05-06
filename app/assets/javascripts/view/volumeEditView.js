module.controller('VolumeEditView', [
	'$scope', 'volume', 'pageService', '$location', '$q', function ($scope, volume, page, $location, $q) {
		page.title = page.constants.message('page.title.stub');

		//

		$scope.wizard = {};

		var activateFn = function (step, wizard) {

		};

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.activateFn = activateFn;
		};

		$scope.updateWizard = function () {
			if ($scope.wizard.newStep) {
				$scope.wizard.newStep.complete = false;
				$scope.wizard.newStep.allow = !!(volume || $scope.wizard.newStep.id === 'volume_edit_overview');

				if ($location.search().page && $scope.wizard.newStep.id.indexOf($location.search().page) > -1) {
					$scope.wizard.activateStep($scope.wizard.newStep);
				} else if ($scope.wizard.newStep.id === 'volume_edit_overview') {
					$scope.wizard.activateStep($scope.wizard.newStep);
				}

				if (angular.isFunction($scope.prepareStep[$scope.wizard.newStep.id]))
					$scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
			}
		};

		//

		$scope.overviewForm = undefined;
		$scope.publicationsForm = undefined;
		$scope.materialsForm = undefined;
		$scope.accessForm = undefined;

		//

		$scope.prepareStep = {
			'volume_edit_overview': function (step) {
				$scope.overviewForm = step.volumeEditOverviewForm;
			},

			'volume_edit_publications': function (step) {
				$scope.publicationsForm = step.volumeEditPublicationsForm;
			},

			'volume_edit_materials': function (step) {
				$scope.materialsForm = step.volumeEditMaterialsForm;
			},

			'volume_edit_access': function (step) {
				$scope.accessForm = step.volumeEditAccessForm;
			},
		};
	}
]);
