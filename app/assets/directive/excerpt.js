'use strict';

module.directive('excerpt', [
  'pageService', function (page) {
    var link = function ($scope, $el, $attr) {
      var obj = $scope[$attr.excerpt].object ? $scope[$attr.excerpt].object : $scope[$attr.excerpt];

      $scope.srcRoute = obj.downloadRoute();

      var excerpt = new page.models.Segment(obj.segment);

      if (isFinite(excerpt.l)) {
        $el.on('loadedmetadata', function () {
          this.currentTime = excerpt.l / 1000;

          $el.off('loadedmetadata');
        });
      }

      if (isFinite(excerpt.u)) {
        $el.on('timeupdate', function () {
          if (this.currentTime < excerpt.u / 1000) {
            return;
          }

          this.pause();
          $el.off('timeupdate');
        });
      }
    };

    return {
      restrict: 'A',
      link: link
    };
  }
]);
