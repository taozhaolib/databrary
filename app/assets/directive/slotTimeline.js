'use strict';

module.directive('slotTimeline', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {

      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotTimeline.html',
      controller: controller,
      controllerAs: 'timeline',
    };
  }
]);
