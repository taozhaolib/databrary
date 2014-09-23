'use strict';

module.directive('sessionMark', [
  'pageService', function (page) {
    var types = {
      exclusion: 'purple',
      pilot: 'red',
    };
    var link = function ($scope, $el) {
      var cat = page.constants.category[$scope.cat.id];
      var type = types[cat.name];

      if (!type)
        return $el.remove();

      var message = "<strong>" + $scope.capitalize(cat.name) + "</strong>: " + page.constants.message('mark.' + cat.name + '.help');
      var extras = [];
      angular.forEach($scope.cat.records, function (r) {
        var i = r.displayName;
        if (i)
          extras.push(i);
      });
      if (extras.length)
        message += ": <em>" + extras.join(", ") + "</em>";

      var tooltip = new page.tooltips({
        message: message,
        type: type,
        $target: $el
      });

      $scope.cat.displayed = true;

      //

      $scope.$on('$destroy', function () {
        page.tooltips.remove(tooltip);
      });

      //

      $scope.mark = cat.name;
      $scope.markClass = 'session-mark-' + $scope.mark;
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/sessionMark.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
