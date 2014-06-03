module.controller('VolumeView', [
	'$scope', 'volume', 'pageService', function ($scope, volume, page) {
		$scope.volume = volume;

		page.display.title = volume.name;
		page.display.toolbarLinks = [
			{
				type: 'yellow',
				html: page.constants.message('volume.edit'),
				url: page.router.volumeEdit(volume),
				access: 'CONTRIBUTE',
				object: volume,
			},
		];

		page.browser.initialize('volume', volume);

		$scope.$watchCollection('volume', function () {
			page.events.talk('panelService-refresh');
		});
	}
]);
