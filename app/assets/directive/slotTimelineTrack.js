'use strict';

module.directive('slotTimelineTrack', [
  function () {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var track = this;
        var ctrl = $scope.ctrl;

        track.asset = $scope.$eval($attrs.asset);
	track.file = $scope.$eval($attrs.file);
	track.drop = $attrs.drop;

        track.classes = function () {
          var cls = [];

          if (!track.asset)
            return cls;

          if (ctrl.hasPosition(track.asset)) {
            cls.push('slot-track-positioned');
          }

          if (ctrl.hasDisplay(track.asset)) {
            cls.push('slot-track-display');
          }

          if (ctrl.isCurrent(track.asset)) {
            cls.push('slot-track-select');
          }

          return cls;
        };

        var left, right;

        track.getPosition = function () {
	  left = (track.asset.segment.l - ctrl.clock.start) / (ctrl.clock.duration - ctrl.clock.start);
	  console.log(left);
	  right = (ctrl.clock.duration - track.asset.segment.u) / (ctrl.clock.duration - ctrl.clock.start);
        };

	if (track.asset)
	  track.getPosition();

        track.styles = function () {
          var styles = {};

          if (left >= 0)
            styles.left = 100 * left + '%';
          if (right <= 1)
            styles.right = 100 * right + '%';

          return styles;
        };

        track.registerMedia = function (media) {
          track.media = media;
        };

        // behavior

        track.select = function () {
          $scope.ctrl.setCurrent(track.asset);
        };
      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotTimelineTrack.html',
      controller: controller,
      controllerAs: 'track',
    };
  }
]);
