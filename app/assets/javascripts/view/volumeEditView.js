module.controller('VolumeEditView', [
	'$scope', 'volume', 'pageService', '$location', function ($scope, volume, page, $location) {
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
			}
		};
	}
]);
