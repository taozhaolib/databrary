'use strict';

module.directive('slotTimeline', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var timeline = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);
        var $now;

        var nowListener = $scope.$on('shout-slotTimelineNow', function (e, $scope, $element) {
          $now = $element;
          nowListener();
          e.stopPropagation();
        });

        timeline.tracks = [];

        timeline.sortTracks = function () {
          timeline.tracks.sort(function sortTracksFn(a, b) {
            if (ctrl.hasPosition(a)) {
              if (ctrl.hasPosition(b)) {
                var aPos = ctrl.hasDuration(a) ? a.segment[0] : a.segment;
                var bPos = ctrl.hasDuration(b) ? b.segment[0] : b.segment;

                if (aPos !== bPos) {
                  return aPos - bPos;
                }

                var aDur = ctrl.hasDuration(a) ? a.segment[1] - a.segment[0] : 0;
                var bDur = ctrl.hasDuration(b) ? b.segment[1] - b.segment[0] : 0;

                return !aDur && !bDur ? a.asset.id - b.asset.id : aDur - bDur;
              } else {
                return -1;
              }
            } else {
              if (ctrl.hasPosition(b)) {
                return 1;
              } else {
                return a.asset.id - b.asset.id;
              }
            }
          });
        };

        timeline.parseTracks = function () {
          timeline.tracks.splice.apply(timeline.tracks, [0, timeline.tracks.length].concat(page.$filter('toArray')(ctrl.slot.assets)));
          timeline.time.left = ctrl.clock.start;
          timeline.time.right = ctrl.clock.duration;
          timeline.sortTracks();
        };

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

        timeline.parseTracks();
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
