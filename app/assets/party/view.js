'use strict';

app.controller('party/view', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;

    page.display.title = party.name;
    if (party.checkPermission(page.permission.EDIT))
      page.display.toolbarLinks.push({
        type: 'yellow',
        html: page.constants.message('party.edit'),
        url: party.editRoute(),
      });
  }
]);
