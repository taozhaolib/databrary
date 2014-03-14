define(['app/config/module'], function (module) {
	'use strict';

	module.factory('AuthPresetService', ['ConstantService', function (constantService) {
		var authPresetService = {};

		//

		authPresetService.get = function (party, other, request) {
			if(!angular.isObject(party) || !angular.isObject(other))
				return;

			if (request) {
				if (other.institution || other.id == 0)
					return constantService.data.preset.institution.slice(0, 2);
				else
					return constantService.data.preset.individual.slice(0, 3);
			} else {
				if (party.institution || party.id == 0)
					return constantService.data.preset.institution;
				else
					return constantService.data.preset.individual;
			}
		};

		authPresetService.set = function (party, preset) {
			party.preset = preset;

			if (preset && angular.isDefined(preset.inherit)) {
				party.inherit = preset.inherit;
				party.direct = preset.direct;
			}
		};

		//

		return authPresetService;
	}]);
});
