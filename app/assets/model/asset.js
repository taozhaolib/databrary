'use strict';

module.factory('asset', [
  '$resource', 'routerService', function ($resource, router) {
    var asset = $resource('/api/asset/:id');

    asset.fileAddedImmediateUpload = function (file) {
      file.pause();
      router.http(router.controllers.AssetApi.uploadStart,
	  file.name,
          file.size
      ).then(function (res) {
        file.uniqueIdentifier = res.data;
      }).then(function () {
        file.resume();
      });
    };

    asset.replace = function (assetId, data) {
      return router.http(router.controllers.AssetApi.replace, assetId, data);
    };

    asset.newAssetCall = function (volumeId, data) {
      return router.http(router.controllers.AssetApi.upload, volumeId, data);
    };

    asset.flowOptions = {
      target: router.controllers.AssetApi.uploadChunk().url,
      method: 'octet',
      simultaneousUploads: 3,
      testChunks: false,
      chunkRetryInterval: 5000,
      permanentErrors: [400, 403, 404, 415, 500, 501],
      progressCallbacksInterval: 300,
      prioritizeFirstAndLastChunk: true
    };

    return asset;
  }
]);
