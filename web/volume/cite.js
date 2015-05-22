'use strict';

app.directive('citeVolume', [
  'constantService', 'routerService',
  function (constants, router) {
    var link = function ($scope) {
      var authors = '';
      var ai = 0;
      var access = $scope.volume.access && $scope.volume.access[ai];

      function initial(p) {
        if (p)
          authors += p.charAt(0) + '.';
      }

      while (access) {
        var next = $scope.volume.access[++ai];
        if (next && (next.individual || 0) < constants.permission.ADMIN)
          next = undefined;

        if (authors !== '') {
          authors += ', ';
          if (!next)
            authors += ' & ';
        }

        var parts = access.party.name.split(' ');
        authors += parts.pop();

        if (parts.length) {
          authors += ', ';
          parts.forEach(initial);
        }

        access = next;
      }

      $scope.authors = authors;
      $scope.today = new Date();
      $scope.permalink = router.permalink($scope.volume.route());
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/cite.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
