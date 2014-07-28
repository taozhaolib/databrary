'use strict';

module.factory('asset', [
  '$resource', '$http', function ($resource, $http) {
    var asset = $resource('/api/asset/:id');

    asset.upload = function (volume, fd) {
      return $http.post('/api/asset?volume=' + volume.id, fd, {
        transformRequest: angular.identity,
        headers: {
          'Content-Type': undefined
        },
      });
    };

    asset.replace = function (asset, fd) {
      return $http.post('/api/asset/' + asset.asset.id + '/replace', fd, {
        transformRequest: angular.identity,
        headers: {
          'Content-Type': undefined
        },
      });
    };

    return asset;
  }
]);
