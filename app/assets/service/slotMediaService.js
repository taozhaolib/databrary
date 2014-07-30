'use strict';

module.factory('slotMediaService', [
  '$timeout', function ($timeout) {
    // Media

    var Media = function (slot, clock) {
      this.slot = slot;
      this.clock = clock;
      this.begun = undefined;

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

      // TODO: hack until type classes
      this.slot.assets.forEach(function (asset) {
        asset.container = slot.container;
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

    // callbacks

    Media.prototype.callbackPlay = function () {
      var that = this;

      return function () {
        that.media.forEach(function (media) {
          if(media.element.nodeName === 'VIDEO' && angular.isArray(media.asset.segment)) {
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
          if(media.element.nodeName === 'VIDEO' && angular.isArray(media.asset.segment)) {
            media.element.pause();
          }
        });
      };
    };

    // Service

    return Media;
  }
]);
