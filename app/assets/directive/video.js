'use strict';

module.directive('video', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    require: '^?ngController',
    link: function ($scope, $element, $attr, view) {
      $element.on('loadedmetadata', function () {
        var seek = $scope.$eval($attr.seek), stop;
        if (seek instanceof page.models.Segment) {
          stop = seek.u;
          seek = seek.l;
        }

        if (isFinite(seek) && seek > 0) {
          this.currentTime = seek / 1000;
          $element.off('loadedmetadata');
        }

        if (isFinite(stop) && stop > seek) {
          stop /= 1000;
          var checkStop = function() {
            if (this.currentTime < stop)
              return;
            this.pause();
            $element.off('timeupdate', checkStop);
          };
          $element.on('timeupdate', checkStop);
        }

        if (view && view.registerVideo) {
          view.registerVideo($element);
          $scope.$on('$destroy', function () {
            view.deregisterVideo($element);
          });
        }
      });
    }
  }; }
]);
