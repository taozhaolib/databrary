'use strict';

app.directive('broadcast', [
  function () {
    return {
      restrict: 'A',
      scope: true,
      link: function ($scope, $element, $attrs) {
        $scope.$emit($attrs.broadcast || 'broadcast', $scope, $element, $attrs);
      }
    };
  }
]);
