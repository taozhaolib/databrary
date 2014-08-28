'use strict';

module.controller('partyView', [
  '$scope', 'party', 'volumes', 'pageService', function ($scope, party, volumes, page) {
    $scope.party = party;
    $scope.volumes = volumes;

    page.display.title = party.name;
    page.display.toolbarLinks = [
      {
        type: 'yellow',
        html: page.constants.message('party.edit'),
        url: party.editRoute(),
        access: page.permission.CONTRIBUTE,
        object: party,
      },
    ];
  }
]);
