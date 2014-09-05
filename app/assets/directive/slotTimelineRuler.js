'use strict';

module.directive('slotTimelineRuler', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element',
      function ($scope, $element) {
        var ruler = this;

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
