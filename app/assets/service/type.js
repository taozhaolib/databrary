'use strict';

module.factory('typeService', [
  'constantService',
  function (constants) {
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

      if (typeService.isSlot(object)) {
        return 'slot';
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
      return typeService.isVolume(object) && angular.isObject(object.citation);
    };

    typeService.isDataset = function (object) {
      return typeService.isVolume(object) && !angular.isObject(object.citation);
    };

    typeService.isRecord = function (object) {
      return angular.isObject(object) && object.measures;
    };

    typeService.isParty = function (object) {
      return angular.isObject(object) && angular.isDefined(object.institution);
    };

    typeService.isToken = function (object) {
      return angular.isObject(object) && object.auth;
    };

    typeService.isComment = function (object) {
      return angular.isObject(object) && object.text && object.time;
    };

    typeService.isSlot = function (object) {
      return angular.isObject(object) && !object.asset && !object.body && !object.measures && !object.avatar && !object.auth && !object.text;
    };

    //

    typeService.assetProperty = function (object, property, dig) {
      if (!typeService.isAsset(object)) {
        throw new Error('typeService.assetProperty() requires Asset as first argument');
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
      else if (typeService.isSlot(object) || typeService.isComment(object)) {
        segment = object.segment;
      }
      else {
        throw new Error('typeService.segmentString() requires Asset or Session as first argument');
      }

      return segment.toString();
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

    typeService.slotName = function (object) {
      if (!typeService.isSlot(object)) {
        throw new Error('typeService.slotName() requires Slot as first argument');
      }

      return constants.message(object.top ? 'materials' : 'session') + (object.name ? ': ' + object.name : '');
    };

    //Real checking done on server, but some minimal standards (length >= 7) can be checked here
    typeService.eligiblePassword = function (candidate) {
      return angular.isString(candidate) &&
        candidate.length >= 7;
    };

    //
    
    typeService.recordName = function (record) {
      if (!typeService.isRecord(record))
        throw new Error('typeService.recordName() requires Record as first argument');

      var cat = constants.data.category[record.category];
      var ident = [];
      angular.forEach(cat && cat.ident || [constants.data.metricName.ident.id], function (i) {
	if (i in record.measures)
	  ident.push(record.measures[i]);
      });

      return ident.length ? ident.join(', ') : record.id;
    };

    return typeService;
  }
]);
