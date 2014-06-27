module.controller('HelpFormatsView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.help.formats');

	}
]);
