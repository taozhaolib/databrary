'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party',
  function ($scope, display, party) {
    $scope.party = party;
    display.title = party.name;
  }
]);
