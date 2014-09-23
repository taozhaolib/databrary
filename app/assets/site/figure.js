'use strict';

module.directive('figure', [
  'pageService', function (page) {
    var compile = function ($element, $attrs) {
      $element.find('figmedia').first().replaceWith('<div class="figmedia"><div class="figmedia-inner">' + $element.find('figmedia').first().html() + '</div></div>');

      if ($attrs.href) {
        var $wrap = $('<a></a>');

        $wrap.attr('ng-href', $attrs.href);
        $element.attr('href', '');

        if ($attrs.title) {
          $wrap.attr('title', $attrs.title);
          $element.attr('title', '');
        }

        $element.wrapInner($wrap);
      }

      return function ($scope, $element, $attrs) {
        if (angular.isDefined($attrs.fill)) {
          var $inner = $element.find('.figmedia-inner');
          var $img = $element.find('img');

          if (!$img) {
            return page.$log.info('No <img> found in figure.', $element);
          }

          var resizeFn = function () {
            var figW = $element[0].clientWidth;
            var w = $img[0].clientWidth;
            var h = $img[0].clientHeight;
            var calc;

            if (h > w) {
              $inner.css({
                'width': w,
                'left': 'auto',
                'margin-left': 'auto',
              });
            } else {
              calc = w * figW / h;
              $inner.css({
                'width': calc,
                'left': '50%',
                'margin-left': calc / -2,
              });
            }
          };

          $img.one('load', resizeFn);
          $element.on('resize.figure', resizeFn);
        }
      };
    };

    return {
      restrict: 'E',
      priority: 100,
      compile: compile
    };
  }
]);
