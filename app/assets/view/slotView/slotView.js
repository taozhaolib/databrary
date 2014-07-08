module.controller('SlotView', [
	'$scope', 'slot', 'pageService', function ($scope, slot, page) {
		$scope.slot = slot;

		page.display.title = slot.name;
		page.display.toolbarLinks = [];
	}
]);
