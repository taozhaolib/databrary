'use strict';

app.controller('party/view', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;
    page.display.title = party.name;
  }
]);
