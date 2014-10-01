'use strict';

module.directive('citeVolume', [
  'pageService', function (page) {
    var link = function ($scope) {
      var authors = '';
      var ai = 0;
      var access = $scope.volume.access[ai];
      while (access) {
        var next = $scope.volume.access[++ai];
        if (next && (next.individual || 0) < page.permission.ADMIN)
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
          do {
            authors += parts.pop().charAt(0) + '.';
          } while (parts.length);
        }

        access = next;
      }

      $scope.authors = authors;
      $scope.today = new Date();
      $scope.permalink = page.router.permalink($scope.volume.route);
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
