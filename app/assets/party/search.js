'use strict';

app.controller('party/search', [
  '$scope', '$filter', 'displayService', 'parties',
  function ($scope, $filter, display, parties) {
    display.title = 'Users';
    $scope.parties = $filter('orderBy')(parties, 'lastName');
  }
]);
