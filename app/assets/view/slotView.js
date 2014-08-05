'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', function ($scope, slot, page) {
    page.display.title = page.types.slotName(slot);
    page.display.toolbarLinks = [];

    // controller

    var ctrl = {
      slot: slot,

      media: [],
      current: [slot.assets[0]],

      state: {
        selection: null,
      },

      registerMedia: function (media) {
        ctrl.push(media);

        media.$scope.$on('$destroy', function () {
          ctrl.deregisterMedia(media);
        });
      },

      deregisterMedia: function (media) {
        var i = ctrl.indexOf(media);

        if (i > -1) {
          ctrl.splice(i, 1);
        }
      },

      setCurrent: function (asset) {
        ctrl.current[0] = asset;
      },

      select: function (media) {
        ctrl.current[0] = media.asset;
      },

      hasPosition: function (media) {
        var asset = getAsset(media);
        return asset.segment;
      },

      hasDuration: function (media) {
        var asset = getAsset(media);
        return angular.isArray(asset.segment);
      },

      hasDisplay: function (media) {
        var asset = getAsset(media);
        return ['video', 'image'].indexOf(types.assetMimeArray(asset, true)[0]) > -1;
      },

      hasTime: function (media) {
        var asset = getAsset(media);
        return ['video'].indexOf(types.assetMimeArray(asset, true)[0]) > -1;
      },

      isNowPlayable: function (media) {
        var asset = getAsset(media);
        return ctrl.clock.position > asset.segment[0] && ctrl.clock.position < asset.segment[1];
      },

      isPaused: function (media) {
        media = getMedia(media);
        return media.element.paused && media.element.readyState >= 4;
      },
    };

    // clock

    ctrl.clock = page.slotClock(slot, ctrl);

    // helpers

    var getAsset = function (media) {
      return media.element ? media.asset : media;
    };

    var getMedia = function (media) {
      return media.element ? media : ctrl.filter(function (m) {
        return m.asset === media;
      }).pop();
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
              m.element.currentTime = (ctrl.clock.position - m.asset.segment[0]) / 1000;
              m.element.play();
            }
          } else {
            m.element.pause();
          }
        }
      });
    };

    var callbackPlay = function () {
        mediaUpdateFn(ctrl);
    };

    var callbackTime = function () {
        mediaUpdateFn(ctrl);
    };

    var callbackPause = function () {
        ctrl.forEach(function (media) {
          if (ctrl.hasTime(media) && ctrl.hasDuration(media)) {
            media.element.pause();
          }
        });
    };

    ctrl.clock.playFn(callbackPlay);
    ctrl.clock.timeFn(callbackTime);
    ctrl.clock.pauseFn(callbackPause);

    // return

    $scope.ctrl = ctrl;
    return ctrl;
  }
]);
