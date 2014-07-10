module.factory('typeService', [
	'constantService',
	'$window',
	function (constants, $window) {
		var typeService = {};

		//

		typeService.getType = function (object, volumeType) {
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
				return volumeType ? typeService.getVolumeType(object) : 'volume';
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

		typeService.getVolumeType = function (object) {
			if (typeService.isStudy(object)) {
				return 'study';
			}

			if (typeService.isDataset(object)) {
				return 'dataset';
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

		typeService.isStudy = function (object) {
			return typeService.isVolume(object) && object.hasOwnProperty('citation');
		};

		typeService.isDataset = function (object) {
			return typeService.isVolume(object) && !object.hasOwnProperty('citation');
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

			return typeService.segmentJoin(segment);

		};

		typeService.segmentJoin = function (segment) {
			if (typeService.segmentEmpty(segment))
				return '';
			if (angular.isUndefined(segment))
				return ',';
			if (angular.isNumber(segment))
				return segment;
			return  (angular.isNumber(segment[0]) && segment[0] > -Infinity ? Math.floor(segment[0]) : '')
				+ ',' +
				(angular.isNumber(segment[1]) && segment[1] < Infinity ? Math.floor(segment[1]) : '');
		};

		typeService.segmentEmpty = function (seg) {
			return seg === null || angular.isArray(seg) && seg[0] !== null && seg[1] !== null && seg[0] >= seg[1];
		};

		/* always returns a new array */
		var segmentNormalize = function (seg) {
			if (seg === null)
				return [0, -1];
			if (angular.isUndefined(seg))
				return [-Infinity, Infinity];
			if (angular.isNumber(seg))
				return [seg, seg + 0.1];
			return [angular.isNumber(seg[0]) ? seg[0] : -Infinity,
				angular.isNumber(seg[1]) ? seg[1] : Infinity];
		};

		/* may modify and/or return a */
		typeService.segmentIntersect = function (a, b) {
			if (a === null)
				return a;
			b = segmentNormalize(b);
			if (angular.isUndefined(a))
				return b;
			if (angular.isNumber(a) && a >= b[0] && a < b[1])
				return a;
			a[0] = angular.isNumber(a[0]) ? Math.max(a[0], b[0]) : b[0];
			a[1] = angular.isNumber(a[1]) ? Math.min(a[1], b[1]) : b[1];
			return a;
		};

		typeService.overlaps = function (a, b) {
			return !typeService.segmentEmpty(typeService.segmentIntersect(segmentNormalize(a), b));
		}

		/* If segments are disjoint, assume the excluded middle.
		 * may modify and/or return a */
		typeService.segmentUnion = function (a, b) {
			if (angular.isUndefined(a))
				return a;
			b = segmentNormalize(b);
			if (a === null)
				return b;
			if (angular.isNumber(a))
				a = [a, a];
			if (angular.isNumber(a[0])) a[0] = Math.min(a[0], b[0]);
			if (angular.isNumber(a[1])) a[1] = Math.max(a[1], b[1]);
			return a;
		};

		typeService.assetFormat = function (object, dig) {
			return constants.data.format[typeService.assetProperty(object, 'format', dig)];
		};

		typeService.assetMimeArray = function (object, dig) {
			return constants.data.format[typeService.assetProperty(object, 'format', dig)].mimetype.split('/');
		};

		typeService.assetDisplayName = function (object, dig) {
			return typeService.assetProperty(object, 'name', dig) || typeService.assetFormat(object, dig).name;
		};

		typeService.assetIcon = function (object) {
			return '/public/images/filetype/16px/' + typeService.assetFormat(object).extension + '.png';
		};

		typeService.videoSupported = function () {
			return $window.navigator.userAgent.toLowerCase().indexOf('firefox') == -1 || $window.navigator.platform.toLowerCase().indexOf('mac') == -1;
		};

		typeService.slotName = function (object) {
			if (!typeService.isSession(object)) {
				throw new Error('typeService.slotName() requires Slot as first argument');
			}

			return constants.message(object.top ? 'materials' : 'session') + (object.name ? ': ' + object.name : '');
		};

		//

		return typeService;
	}
]);
