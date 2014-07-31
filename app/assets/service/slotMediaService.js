'use strict';

module.factory('slotMediaService', [
  '$timeout', 'typeService', function ($timeout, types) {
    // Media

    var Media = function (slot, clock) {
      var media = this;
      this.slot = slot;
      this.clock = clock;

      Object.defineProperties(this, {
        media: {
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

      this.slot.assets.forEach(function (asset) {
        // TODO: hack until type classes
        asset.container = slot.container;

        if (media.hasDuration(asset)) {
          media.clock.duration = media.clock.duration >= asset.segment[1] ? media.clock.duration : asset.segment[1];
        }
      });

      this.setCurrent(this.slot.assets[0]);

      //

      clock.playFn(this.callbackPlay());
      clock.pauseFn(this.callbackPause());
    };

    Media.prototype.registerMedia = function (media) {
      this.media.push(media);
    };

    Media.prototype.setCurrent = function (asset) {
      this.current[0] = asset;
    };

    // tests

    var getAsset = function (media) {
      return media.element ? media.asset : media;
    };

    Media.prototype.hasPosition = function (media) {
      var asset = getAsset(media);
      return asset.segment;
    };

    Media.prototype.hasDuration = function (media) {
      var asset = getAsset(media);
      return angular.isArray(asset.segment);
    };

    Media.prototype.hasDisplay = function (media) {
      var asset = getAsset(media);
      return ['video', 'image'].indexOf(types.assetMimeArray(asset, true)[0]) > -1;
    };

    Media.prototype.hasTime = function (media) {
      var asset = getAsset(media);
      return ['video'].indexOf(types.assetMimeArray(asset, true)[0]) > -1;
    };

    // callbacks

    Media.prototype.callbackPlay = function () {
      var that = this;

      return function () {
        that.media.forEach(function (media) {
          if(that.hasTime(media) && that.hasDuration(media)) {
            if (that.clock.position > media.asset.segment[0] && that.clock.position < media.asset.segment[1]) {
              media.element.currentTime = (that.clock.position - media.asset.segment[0]) / 1000;
              media.element.play();
            } else {
              media.element.pause();
            }
          }
        });
      };
    };

    Media.prototype.callbackPause = function () {
      var that = this;

      return function () {
        that.media.forEach(function (media) {
          if(that.hasTime(media) && that.hasDuration(media)) {
            media.element.pause();
          }
        });
      };
    };

    // Service

    return Media;
  }
]);
