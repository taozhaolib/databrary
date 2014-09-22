'use strict';

module.directive('viewerAspect', [
  'pageService', function (page) {
    return {
      restrict: 'A',
      link: function ($scope, $element, $attr) {
	var vr = $scope.$eval($attr.viewerAspect);

        function resize() {
          var w = $element.width(), h = w * vr;
	  $element.height(h);

          var $m = $element.find('video, img, aside').first();
          if (!$m)
            return;

          var mw = $m.width(), mh = $m.height(), mr = mh / mw;
          if (mr < vr) {
            $m.width(h / mr);
            $m.height(h);
          } else {
            $m.height(w * mr);
            $m.width(w);
          }
        }

        page.$w.resize(resize);
	resize();
      }
    };
  }
]);
