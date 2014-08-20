'use strict';

module.controller('homeView', [
  '$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
    page.display.title = page.constants.message('page.title.home');

    var lastName = function (p) {
      var s = p.name;
      return s ? s.substr(s.lastIndexOf(' ')+1) : '';
    };

    $scope.parties = page.$filter('orderBy')(parties, lastName);
    $scope.volume = volume;

  }
]);
