module.controller('SlotView', [
	'$scope', 'slot', 'pageService', function ($scope, slot, page) {
		$scope.slot = slot;
		console.log(slot);

		page.display.title = slot.name;
		page.display.toolbarLinks = [];
	}
]);
