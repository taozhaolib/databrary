'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'volumes',
  function ($scope, display, party, volumes) {
    $scope.party = party;
    $scope.volumes = volumes;
    display.title = party.name;
  }
]);
