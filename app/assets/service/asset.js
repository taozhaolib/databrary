'use strict';

module.factory('assetService', [
  'routerService', function (router) { return {
    assetStart: function (file) {
      file.pause();
      return router.http(router.controllers.AssetApi.uploadStart, {
	filename: file.name,
	size: file.size
      }).then(function(res){
	file.uniqueIdentifier = res.data;
      });
    },

    flowOptions: {
      target: router.controllers.AssetApi.uploadChunk().url,
      method: 'octet',
      simultaneousUploads: 3,
      testChunks: false,
      chunkRetryInterval: 5000,
      permanentErrors: [400, 403, 404, 409, 415, 500, 501],
      progressCallbacksInterval: 500,
      prioritizeFirstAndLastChunk: true
    }
  }; }
]);
