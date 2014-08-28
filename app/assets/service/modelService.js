'use strict';

module.factory('modelService', [
  '$injector', function ($injector) {
    var models = {};

    //

    angular.forEach([
      'Party',
      'Login',

      'analytic',
      'asset',
      'comment',
      'cite',
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
