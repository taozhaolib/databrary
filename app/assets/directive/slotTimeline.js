'use strict';

module.directive('slotTimeline', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var timeline = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);

        timeline.tracks = [];

        timeline.sortTracks = function () {
          timeline.tracks.sort(function sortTracksFn(a, b) {
            if (ctrl.media.hasPosition(a)) {
              if (ctrl.media.hasPosition(b)) {
                var aPos = ctrl.media.hasDuration(a) ? a.segment[0] : a.segment;
                var bPos = ctrl.media.hasDuration(b) ? b.segment[0] : b.segment;

                if (aPos !== bPos) {
                  return aPos - bPos;
                }

                var aDur = ctrl.media.hasDuration(a) ? a.segment[1] - a.segment[0] : 0;
                var bDur = ctrl.media.hasDuration(b) ? b.segment[1] - b.segment[0] : 0;

                return !aDur && !bDur ? a.asset.id - b.asset.id : aDur - bDur;
              } else {
                return -1;
              }
            } else {
              if (ctrl.media.hasPosition(b)) {
                return 1;
              } else {
                return a.asset.id - b.asset.id;
              }
            }
          });
        };

        timeline.parseTracks = function () {
          timeline.tracks.splice.apply(timeline.tracks, [0, timeline.tracks.length].concat(page.$filter('toArray')(ctrl.slot.assets)));
          timeline.sortTracks();
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
