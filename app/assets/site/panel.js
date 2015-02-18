'use strict';

app.directive('panel', [
  'constantService',
  function (constants) {
    var link = function ($scope, $element, $attrs) {
      if (angular.isDefined($attrs.id)) {
        $scope.id = $attrs.id;
        $element.addClass($attrs.id);
      } else {
        $scope.id = '';
      }

      $scope.title = constants.message($attrs.panelTitle);
      $scope.top = 'top' in $attrs;
      $scope.enabled = true;

      //

      $scope.getPanelClasses = function () {
        var classes = {};

        classes.panel = true;
        classes[$scope.id] = true;

        return classes;
      };

      //

      if ($scope.refreshPanel)
        $scope.refreshPanel();
    };

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'site/panel.html',
      transclude: true,
      replace: true,
      priority: 100,
      link: link
    };
  }
]);
