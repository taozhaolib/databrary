'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', function ($scope, slot, page) {
    page.display.title = page.types.slotName(slot);
    page.display.toolbarLinks = [];

    $scope.ctrl = {
      slot: slot,
      clock: new page.clock(),
      state: {
        selection: null,
      },
    };
  }
]);
