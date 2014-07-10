'use strict';

module.controller('slotView', [
	'$scope', 'slot', 'pageService', function ($scope, slot, page) {
		$scope.slot = slot;

		page.display.title = page.types.slotName(slot);
		page.display.toolbarLinks = [];
	}
]);
