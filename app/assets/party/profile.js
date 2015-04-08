 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'volumes', 'pageService',
  function ($scope, display, party, volumes, page) {
    $scope.party = party;
    $scope.volumes = volumes;
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;

  }
]);
