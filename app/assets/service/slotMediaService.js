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
      clock.timeFn(this.callbackTime());
      clock.pauseFn(this.callbackPause());
    };

    Media.prototype.registerMedia = function (media) {
      var that = this;
      this.media.push(media);

      media.$scope.$on('$destroy', function () {
        that.deregisterMedia(media);
      });
    };

    Media.prototype.deregisterMedia = function (media) {
      var i = this.media.indexOf(media);

      if (i > -1) {
        this.media.splice(i, 1);
      }
    };

    Media.prototype.setCurrent = function (asset) {
      this.current[0] = asset;
    };

    Media.prototype.select = function (media) {
      this.current[0] = media.asset;
    };

    // tests

    var getAsset = function (media) {
      return media.element ? media.asset : media;
    };

    var getMedia = function (media) {
      return media.element ? media : this.media.filter(function (m) {
        return m.asset === media;
      }).pop();
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

    Media.prototype.isNowPlayable = function (media) {
      var asset = getAsset(media);
      return this.clock.position > asset.segment[0] && this.clock.position < asset.segment[1];
    };

    Media.prototype.isPaused = function (media) {
      media = getMedia(media);
      return media.element.paused && media.element.readyState >= 4;
    };

    // callbacks

    var mediaUpdateFn = function (media) {
      var asset = !media.hasPosition(media.current[0]) ? media.current[0] : undefined;

      media.media.forEach(function (m) {
        if (asset) {
          if (m.asset === asset) {
            if (media.isPaused(m)) {
              m.element.currentTime = 0;
              m.element.play();
            }
          } else {
            m.element.pause();
          }
        } else if (media.hasTime(m) && media.hasDuration(m)) {
          if (media.isNowPlayable(m)) {
            if (media.isPaused(m)) {
              m.element.currentTime = (media.clock.position - m.asset.segment[0]) / 1000;
              m.element.play();
            }
          } else {
            m.element.pause();
          }
        }
      });
    };

    Media.prototype.callbackPlay = function () {
      var that = this;

      return function () {
        mediaUpdateFn(that);
      };
    };

    Media.prototype.callbackTime = function () {
      var that = this;

      return function () {
        mediaUpdateFn(that);
      };
    };

    Media.prototype.callbackPause = function () {
      var that = this;

      return function () {
        that.media.forEach(function (media) {
          if (that.hasTime(media) && that.hasDuration(media)) {
            media.element.pause();
          }
        });
      };
    };

    // Service

    return Media;
  }
]);
