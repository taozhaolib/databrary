'use strict';

module.directive('slotTimelineTrack', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var track = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);

        track.asset = page.$parse($attrs.asset)($scope);
        track.spare = angular.isDefined($attrs.spare);

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
          if (ctrl.hasDuration(track.asset)) {
            left = (track.asset.segment[0] - ctrl.clock.start) / (ctrl.clock.duration - ctrl.clock.start);
            right = (ctrl.clock.duration - track.asset.segment[1]) / (ctrl.clock.duration - ctrl.clock.start);
          } else if (ctrl.hasPosition(track.asset)) {
            left = (track.asset.segment - ctrl.clock.start) / (ctrl.clock.duration - ctrl.clock.start);
            right = 1 - left;
          }

          left = left ? left * 100 : 0;
          right = right ? right * 100 : 0;
        };

        track.getPosition();

        track.styles = function () {
          var styles = {};

          if (angular.isDefined(left)) {
            styles.left = left + '%';
          }
          if (angular.isDefined(right)) {
            styles.right = right + '%';
          }

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
