'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', function ($scope, slot, page) {
    $scope.slot = slot;
    $scope.clock = new page.clock();

    page.display.title = page.types.slotName(slot);
    page.display.toolbarLinks = [];
  }
]);
