'use strict';

module.directive('slotMedia', [
  'pageService', function (page) {
    var pre = function ($scope, $element, $attrs, slotTimelineTrack) {
      var media = {
        asset: page.$parse($attrs.asset)($scope),
        $scope: $scope,
      };
      media.id = 'slot-media-' + media.asset.asset.id;

      $scope.media = media;

      /* why is this in a timeout? */
      page.$timeout(function () {
        $scope.ctrl.registerMedia(media);

        if (slotTimelineTrack) {
          slotTimelineTrack.registerMedia(media);
        }
      });
    };

    //

    return {
      restrict: 'E',
      scope: true,
      require: '?^slotTimelineTrack',
      templateUrl: 'slotMedia.html',
      link: {
        pre: pre
      },
    };
  }
]);
