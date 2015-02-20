'use strict';

app.directive('panel', [
  'constantService',
  function (constants) {
    var link = function ($scope, $element, $attrs) {
      if ('id' in $attrs)
        $scope.id = $attrs.id;
      else
        $scope.id = '';

      $scope.title = constants.message($attrs.panelTitle);
      $scope.top = 'top' in $attrs;
      $scope.edit = $attrs.editLink;
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
