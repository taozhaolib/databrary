'use strict';

module.directive('scroll', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var timer = page.$timeout(angular.noop);
      var timeout = $.isNumeric($attrs.scrollTimeout) ? parseInt($attrs.scrollTimeout) : 100;

      $element.on('resize scroll', function ($event) {
        page.$timeout.cancel(timer);
        timer = page.$timeout(function () {
          $scope.$eval($attrs.scroll, {
            $scroll: {
              $event: $event,
              $scope: $scope,
              $element: $element,
              $attrs: $attrs
            }
          });
        }, timeout);
      });
    };

    return {
      restrict: 'A',
      link: link
    };
  }
]);
