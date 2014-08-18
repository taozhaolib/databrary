'use strict';

module.directive('slotMedia', [
  'pageService', function (page) {
    var pre = function ($scope, $element, $attrs, slotTimelineTrack) {
      var media = {
        asset: page.$parse($attrs.asset)($scope),
        $scope: $scope,
      };

      $scope.media = media;

      page.$timeout(function () {
        media.$element = $element.find('video, img, aside');
        media.element = media.$element[0];

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
