module.factory('modelService', [
	'$injector', function ($injector) {
		var models = {};

		//

		angular.forEach([
			'Analytic',
			'Asset',
			'Comment',
			'Cite',
			'Party',
			'PartyAuthorize',
			'Record',
			'Scraper',
			'Slot',
			'SlotAsset',
			'Tag',
			'Volume',
			'VolumeAccess',
		], function (dependency) {
			models[dependency] = $injector.get(dependency);
		});

		//

		return models;
	}
]);
