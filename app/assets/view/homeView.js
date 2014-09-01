'use strict';

module.controller('homeView', [
  '$scope', 'parties', 'volume', 'pageService', function ($scope, parties, volume, page) {
    page.display.title = page.constants.message('page.title.home');
    $scope.parties = page.$filter('orderBy')(parties, 'lastName');
    $scope.volume = volume;
  }
]);
