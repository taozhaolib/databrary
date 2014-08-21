'use strict';

module.directive('slotTimelineRuler', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var ruler = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);

        ruler.draw = function () {
          var $ruler = $element.find('.slot-timeline-canvas-ruler');
          var ruler = $ruler[0];

          if (!ruler.getContext) {
            return;
          }

          ruler.getContext('2d');

          // calculate zoom

          // draw ticks

          // position

        };

        page.$timeout(function () {
          ruler.draw();
        });
      }
    ];

    //

    return {
      restrict: 'E',
      scope: false,
      templateUrl: 'slotTimelineRuler.html',
      controller: controller,
      controllerAs: 'ruler',
    };
  }
]);
