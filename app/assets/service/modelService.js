'use strict';

module.factory('modelService', [
	'$injector', function ($injector) {
		var models = {};

		//

		angular.forEach([
			'analytic',
			'asset',
			'comment',
			'cite',
			'party',
			'partyAuthorize',
			'record',
			'slot',
			'slotAsset',
			'tag',
			'volume',
			'volumeAccess',
		], function (dependency) {
			models[dependency] = $injector.get(dependency);
		});

		//

		return models;
	}
]);
