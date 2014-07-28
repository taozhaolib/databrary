'use strict';

module.directive('expose', [
  function () {
    return {
      restrict: 'A',
      link: {
	pre: function ($scope, $element, $attrs) {
	  $scope.$element = $element;
	  $scope.$attrs = $attrs;
	}
      }
    };
  }
]);
