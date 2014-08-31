'use strict';

module.factory('constantService', [
  '$log', 'constantData', function ($log, constantData) {
    var constants = {
      data: constantData,
      regex: {
        doi: /^(?:[dD][oO][iI]:|(?:http:\/\/)?dx\.doi\.org\/)?(10\.[0-9\.]+\/\S+)\s*$/,
      },
    };

    //

    var invertArray = function (data) {
      var r = {};
      angular.forEach(data, function (x, id) {
        r[x] = id;
      });
      return r;
    };

    var invertBy = function (data, field) {
      var r = {};
      angular.forEach(data, function (x) {
        if (field in x)
          r[x[field]] = x;
      });
      return r;
    };

    var makePresets = function () {
      constants.data.accessGlobal = [
        ['NONE', 'NONE'],
        ['NONE', 'SHARED'],
        ['PUBLIC', 'SHARED']
      ];
      constants.data.accessGlobal.parties = [
        constants.data.party.NOBODY,
        constants.data.party.ROOT
      ];
    };

    var update = function () {
      constants.data.permissionName = invertArray(constants.data.permission);
      constants.data.classificationName = invertArray(constants.data.classification);
      constants.data.consentName = invertArray(constants.data.consent);
      constants.data.categoryName = invertBy(constants.data.category, "name");
      constants.data.metricName = invertBy(constants.data.metric, "name");

      /* convenient aliases: */
      constants.data.permissionName.CONTRIBUTE = constants.data.permissionName.EDIT;
      constants.data.permissionName.SUPER = constants.data.permission.length;

      /* backwards compatibility: */
      angular.forEach(constants.data.party, function (party, name) {
	var uname = name.toUpperCase();
	if (angular.isObject(party) && name !== uname)
	  constants.data.party[uname] = party.id;
      });

      angular.forEach(constants.data.format, function (fmt) {
	var m = fmt.mimetype;
	fmt.type = m.slice(0, m.indexOf('/'));
      });

      makePresets();

      Object.freeze(constants.data);
    };

    constants.message = function (key /*, args...*/) {
      var msg = constants.data.messages[key];

      if (!angular.isDefined(msg)) {
        $log.info('Message key [' + key + '] is undefined.');
        return '[' + key + ']';
      }

      for (var i = 1, length = arguments.length; i < length; i++) {
        msg = msg.replace('{' + (i - 1) + '}', arguments[i], 'g');
      }

      return msg;
    };

    //

    update();

    return constants;
  }
]);
