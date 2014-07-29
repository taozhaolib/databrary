'use strict';

module.directive('slotTimeline', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var timeline = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);
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
