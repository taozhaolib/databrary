'use strict';

module.directive('wizardStep', [
  'pageService', function (page) {
    return {
      restrict: 'E',
      templateUrl: 'wizardStep.html',
      require: '^wizard',
      scope: {},
      transclude: true,
      link: function ($scope, $element, $attrs, wizard) {
	$scope.page = page;
        $scope.name = $attrs.name;
        $scope.id = wizard.name + '-' + $scope.name;

	$scope.allow = $attrs.allow === undefined || $scope.$parent.$eval($attrs.allow);
        var enabled = $attrs.enabled === undefined || $scope.$parent.$eval($attrs.enabled);

	if (enabled)
	  wizard.addStep($scope);
      }
    };
  }
]);
