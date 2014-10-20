'use strict';

app.directive('wizardStep', [
  function () {
    return {
      restrict: 'E',
      templateUrl: 'site/wizardStep.html',
      require: '^wizard',
      scope: {},
      transclude: true,
      link: {
        pre: function ($scope, $element, $attrs, wizard) {
          $scope.name = $attrs.name;
          $scope.id = wizard.name + '-' + $scope.name;
        },
        post: function ($scope, $element, $attrs, wizard) {
          $scope.$scope = $scope.$$childHead;

          $scope.allow = $attrs.allow === undefined || $scope.$parent.$eval($attrs.allow);
          $scope.complete = $attrs.complete && $scope.$parent.$eval($attrs.complete);
          var enabled = $attrs.enabled === undefined || $scope.$parent.$eval($attrs.enabled);

          if (enabled)
            wizard.addStep($scope);
        }
      }
    };
  }
]);
