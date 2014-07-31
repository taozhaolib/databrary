'use strict';

module.directive('slotMedia', [
  'pageService', function (page) {
    var pre = function ($scope, $element, $attrs) {
      var $media;
      var media = {
        asset: page.$parse($attrs.asset)($scope),
        $scope: $scope,
      };

      $scope.media = media;

      page.$timeout(function () {
        media.$element = $element.find('video, img');
        media.element = media.$element[0];

        $scope.ctrl.media.registerMedia(media);
      });

      // controller

      return media;
    };

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotMedia.html',
      link: {
        pre: pre
      },
    };
  }
]);
