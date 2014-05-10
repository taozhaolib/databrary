module.factory('typeService', [
	'constantService',
	function (constants) {
		var typeService = {};

		//

		typeService.getType = function (object) {
			if (!angular.isObject(object)) {
				return undefined;
			}

			if (typeService.isParty(object)) {
				return 'party';
			}

			if (typeService.isRecord(object)) {
				return 'record';
			}

			if (typeService.isVolume(object)) {
				return 'volume';
			}

			if (typeService.isAsset(object)) {
				return 'asset';
			}

			if (typeService.isToken(object)) {
				return 'token';
			}

			if (typeService.isComment(object)) {
				return 'comment';
			}

			if (typeService.isSession(object)) {
				return 'session';
			}

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

		typeService.isComment = function (object) {
			return angular.isObject(object) && object.text && object.time;
		};

		typeService.isSession = function (object) {
			return angular.isObject(object) && !object.asset && !object.body && !object.measures && !object.avatar && !object.auth && !object.text;
		};

		//

		typeService.assetProperty = function (object, property, dig) {
			if (!typeService.isAsset(object)) {
				throw new Error('typeService.assetFormat() requires Asset as first argument');
			}

			if (dig === true) {
				return object.asset[property];
			}
			else if (dig === false || property in object) {
				return object[property];
			}
			else {
				return object.asset[property];
			}
		};

		typeService.segmentString = function (object, dig) {
			var segment;

			if (typeService.isAsset(object)) {
				segment = typeService.assetProperty(object, 'segment', dig);
			}
			else if (typeService.isSession(object) || typeService.isComment(object)) {
				segment = object.segment;
			}
			else {
				throw new Error('typeService.segmentString() requires Asset or Session as first argument');
			}

			if (!segment) {
				return ',';
			}

			if (!angular.isArray(segment)) {
				return segment;
			}

			if (segment[0] === null) {
				return ',' + segment[1];
			}

			if (segment[1] === null) {
				return segment[0] + ',';
			}

			return segment.join(',');
		};

		typeService.assetFormat = function (object, dig) {
			return constants.get('format', typeService.assetProperty(object, 'format', dig));
		}

		typeService.assetDisplayName = function (object, dig) {
			return typeService.assetProperty(object, 'name', dig) || typeService.assetFormat(object, dig).name;
		};

		//

		return typeService;
	}
]);
