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
			{
				type: 'orange',
				html: 'access',
				url: page.router.volumeAccess(volume),
				access: 'ADMIN',
				object: volume,
			},
		];console.log(page.display.toolbarLinks);

		$scope.browser.initialize('volume', volume);

		$scope.$watchCollection('volume', function () {
			page.events.talk('panelService-refresh');
		});
	}
]);
