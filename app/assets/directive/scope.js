'use strict';

module.directive('scope', [
  function () {
    return {
      restrict: 'AE',
      scope: true,
      link: function ($scope, $element, $attrs) {
        if (angular.isString($attrs.scope)) {
          $scope.$emit($attrs.scope, $scope, $element, $attrs);
        }
      }
    };
  }
]);
