'use strict';

module.directive('emit', [
  function () {
    return {
      restrict: 'A',
      scope: true,
      link: function ($scope, $element, $attrs) {
        $scope.$emit($attrs.emit || 'emit', $scope, $element, $attrs);
      }
    };
  }
]);
