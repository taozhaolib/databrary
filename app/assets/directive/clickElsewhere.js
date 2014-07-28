'use strict';

module.directive('clickElsewhere', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      $element.bind('click', function ($event) {
	$event.stopPropagation();
      });

      page.$document.bind('click', function () {
	$scope.$apply($attrs.clickElsewhere);
      });
    };

    return {
      restrict: 'A',
      link: link
    };
  }
]);
