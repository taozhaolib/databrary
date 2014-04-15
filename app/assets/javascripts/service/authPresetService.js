module.factory('authPresetService', [
	'pageService', function (page) {
		var authPresetService = {};

		//

		authPresetService.get = function (party, other, request) {
			if (!angular.isObject(party) || !angular.isObject(other))
				return [];

			if (request) {
				if (other.institution || other.id == 0)
					return page.constants.data.preset.institution.slice(0, 2);
				else
					return page.constants.data.preset.institution.slice(0, 1);
			} else {
				if (party.institution || party.id == 0)
					return page.constants.data.preset.institution;
				else
					return page.constants.data.preset.individual;
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
	}
]);
