'use strict';

module.factory('typeService', [
  function () {
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

    //Real checking done on server, but some minimal standards (length >= 7) can be checked here
    typeService.eligiblePassword = function (candidate) {
      return angular.isString(candidate) &&
        candidate.length >= 7;
    };

    return typeService;
  }
]);
