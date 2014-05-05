module.controller('VolumeEditView', [
	'$scope', 'volume', 'pageService', function ($scope, volume, page) {
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
//			activate = angular.isUndefined(activate) ? true : activate;

//			if ($scope.wizard.newStep) {
//				$scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
//			}

//			angular.forEach($scope.wizard.steps, function (step) {
//				$scope.updateStep[step.id](step, activate);
//			});
		};

		//


	}
]);
