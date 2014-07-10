'use strict';

module.factory('modelService', [
	'$injector', function ($injector) {
		var models = {};

		//

		angular.forEach([
			'Analytic',
			'Asset',
			'Comment',
			'CrossCite',
			'Party',
			'PartyAuthorize',
			'Record',
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
