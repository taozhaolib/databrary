define(['config/module'], function (module) {
	'use strict';

	module.factory('TypeService', ['$rootScope', function ($rootScope) {
		var typeService = {};

		//

		typeService.getType = function (object) {
			if (!angular.isObject(object))
				return undefined;

			if (typeService.isParty(object))
				return 'party';

			if (typeService.isRecord(object))
				return 'record';

			if (typeService.isVolume(object))
				return 'volume';

			if (typeService.isAsset(object))
				return 'asset';

			if (typeService.isToken(object))
				return 'token';

			if (typeService.isSession(object))
				return 'session';

			return undefined;
		};

		//

		typeService.isAsset = function (object) {
			return angular.isObject(object) && object.asset;
		};

		typeService.isVolume = function (object) {
			return angular.isObject(object) && object.hasOwnProperty('body');
		};

		typeService.isRecord = function (object) {
			return angular.isObject(object) && object.measures;
		};

		typeService.isParty = function (object) {
			return angular.isObject(object) && object.avatar;
		};

		typeService.isToken = function (object) {
			return angular.isObject(object) && object.auth;
		};

		typeService.isSession = function (object) {
			return angular.isObject(object) && !object.asset && !object.body && !object.measures && !object.avatar && !object.auth;
		};

		//

		return typeService;
	}]);
});
