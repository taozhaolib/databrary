'use strict';

module.directive('slotTimeline', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element',
      function ($scope, $element) {
        var timeline = this;
        var ctrl = $scope.ctrl;
	ctrl.timeline = timeline;
        var $now;

        var nowListener = $scope.$on('shout-slotTimelineNow', function (e, $scope, $element) {
          $now = $element;
          nowListener();
          e.stopPropagation();
        });

        timeline.time = {
          left: 0,
          right: null,
        };

        // listeners

        var nowMoveFn = function (clock) {
          if ($now && $now.length > 0)
            $now.css({
              left: (((clock.position - clock.start) / (clock.duration - clock.start)) * 100) + '%',
            });
        };

        ctrl.clock.playFn(nowMoveFn);
        ctrl.clock.pauseFn(nowMoveFn);
        ctrl.clock.timeFn(nowMoveFn);

        // jump

        timeline.jump = function ($event) {
          var $el = $element.find('.slot-timeline');
          var position = ($event.clientX - $el.offset().left) / $el.outerWidth();
          ctrl.clock.jump((position * (ctrl.clock.duration - ctrl.clock.start)) + ctrl.clock.start);
        };

        // init

        $scope.parseTracks();
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
