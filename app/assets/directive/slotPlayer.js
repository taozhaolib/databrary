'use strict';

module.directive('slotPlayer', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element',
       function ($scope, $element) {
        var player = this;

        var $viewport;
        var viewportW;
        var vr = 9 / 16;

        player.resize = function () {
          var newW = $viewport.width();

          if (newW !== viewportW) {
            viewportW = newW;
            $viewport.height(viewportW * vr);
          }

          var $m = $viewport.find('video, img, aside').first();

          if (!$m) {
            return;
          }

          var w = $viewport.width(), h = $viewport.height();
          var mw = $m.width(), mh = $m.height(), mr = mh / mw;

          if (mr < vr) {
            $m.width(h / mr);
            $m.height(h);
          } else {
            $m.height(w * mr);
            $m.width(w);
          }
        };

        page.$w.resize(player.resize);
        $scope.$watch(function () {
          if ($viewport) {
            player.resize();
          }
        });
        page.$timeout(function () {
          $viewport = $element.find('.player-main-viewport');
          player.resize();
        });
      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotPlayer.html',
      controller: controller,
      controllerAs: 'player',
    };
  }
]);
