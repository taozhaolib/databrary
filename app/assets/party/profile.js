 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService',
  function ($scope, display, party, page) {
    $scope.party = party;
    $scope.volumes = party.volumes;
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
