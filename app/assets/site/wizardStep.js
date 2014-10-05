'use strict';

app.directive('wizardStep', [
  'pageService', function (page) {
    return {
      restrict: 'E',
      templateUrl: 'site/wizardStep.html',
      require: '^wizard',
      scope: {},
      transclude: true,
      link: function ($scope, $element, $attrs, wizard) {
        $scope.page = page;
        $scope.name = $attrs.name;
        $scope.id = wizard.name + '-' + $scope.name;
        $scope.$scope = $scope.$$nextSibling;

        $scope.allow = $attrs.allow === undefined || $scope.$parent.$eval($attrs.allow);
        $scope.complete = $attrs.complete && $scope.$parent.$eval($attrs.complete);
        var enabled = $attrs.enabled === undefined || $scope.$parent.$eval($attrs.enabled);

        if (enabled)
          wizard.addStep($scope);
      }
    };
  }
]);
