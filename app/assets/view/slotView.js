'use strict';

module.controller('SlotView', [
	'$scope', 'slot', 'pageService', function ($scope, slot, page) {
		$scope.slot = slot;

		page.display.title = page.types.slotName(slot);
		page.display.toolbarLinks = [];
	}
]);
