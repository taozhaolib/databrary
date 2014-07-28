'use strict';

module.directive('portrait', [
  'pageService', function (page) {
    return {
      restrict: 'E',
      templateUrl: 'portrait.html',
      link: {
	pre: function ($scope, $element, $attrs) {
	  $scope.portraitExtra = page.$parse($attrs.extra)($scope);
	},
	post: function ($scope, $element, $attrs) {
	  $element.find('.portrait').addClass($attrs.class);
	  $element.attr('class', '');
	}
      }
    };
  }
]);
