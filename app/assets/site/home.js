'use strict';

app.controller('site/home', [
  '$scope', '$filter', 'constantService', 'displayService', 'investigators', 'users', 'volume', 'tags',
  function ($scope, $filter, constants, display, investigators, users, volume, tags) {
    display.title = constants.message('welcome.title');
    $scope.investigators = $filter('orderBy')(investigators, 'lastName');
    $scope.users = $filter('orderBy')(users, 'lastName');
    $scope.volume = volume;
    $scope.tags = tags;
  }
]);
