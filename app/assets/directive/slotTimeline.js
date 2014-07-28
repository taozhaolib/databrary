'use strict';

module.directive('slotTimeline', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var timeline = this;
        timeline.slot = page.$parse($attrs.slot)($scope);
        timeline.clock = page.$parse($attrs.clock)($scope);
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
