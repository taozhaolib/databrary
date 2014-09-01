'use strict';

module.factory('constantService', [
  '$log', '$sce', 'constantData', function ($log, $sce, constants) {
    
    constants.regex = {
      doi: /^(?:[dD][oO][iI]:|(?:http:\/\/)?dx\.doi\.org\/)?(10\.[0-9\.]+\/\S+)\s*$/,
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

    constants.permissionName = invertArray(constants.permission);
    constants.classificationName = invertArray(constants.classification);
    constants.consentName = invertArray(constants.consent);
    constants.categoryName = invertBy(constants.category, "name");
    constants.metricName = invertBy(constants.metric, "name");

    /* convenient aliases: */
    constants.permissionName.CONTRIBUTE = constants.permissionName.EDIT;
    constants.permissionName.SUPER = constants.permission.length;

    /* backwards compatibility: */
    angular.forEach(constants.party, function (party, name) {
      var uname = name.toUpperCase();
      if (angular.isObject(party) && name !== uname)
	constants.party[uname] = party.id;
    });

    angular.forEach(constants.format, function (fmt) {
      var m = fmt.mimetype;
      fmt.type = m.slice(0, m.indexOf('/'));
    });

    constants.accessGlobal = [
      ['NONE', 'NONE'],
      ['NONE', 'SHARED'],
      ['PUBLIC', 'SHARED']
    ];
    constants.accessGlobal.parties = [
      constants.party.NOBODY,
      constants.party.ROOT
    ];

    constants.message = function (key /*, args...*/) {
      var msg = constants.messages[key];

      if (!angular.isDefined(msg)) {
        $log.info('Message key [' + key + '] is undefined.');
        return '[' + key + ']';
      }

      for (var i = 1, length = arguments.length; i < length; i++)
        msg = msg.replace('{' + (i - 1) + '}', $sce.getTrustedHtml(arguments[i]), 'g');

      return $sce.trustAsHtml(msg);
    };

    // TODO: deepFreeze
    return Object.freeze(constants);
  }
]);
