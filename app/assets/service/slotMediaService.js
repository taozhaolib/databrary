'use strict';

module.factory('slotMediaService', [
  '$timeout', function ($timeout) {
    // Media

    var Media = function (slot) {
      this.slot = slot;

      Object.defineProperties(this, {
        assets: {
          value: [],
          writable: false,
          configurable: false
        },

        current: {
          value: [],
          writable: false,
          configurable: false
        },
      });

      this.parseAssets();
      this.setCurrent(this.assets[0].asset);
    };

    Media.prototype.parseAssets = function () {
      if (!this.slot.assets)
        return;

      var that = this;

      this.assets.splice(0, this.assets.length);

      this.slot.assets.forEach(function (asset) {
        asset.container = that.slot.container;

        that.assets.push({
          asset: asset
        });
      });
    };

    Media.prototype.setCurrent = function (asset) {
      this.current[0] = asset;
    };

    // Service

    return Media;
  }
]);
