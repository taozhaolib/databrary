'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', function ($scope, slot, page) {
    page.display.title = page.types.slotName(slot);
    page.display.toolbarLinks = [];

    // helpers

    var getAsset = function (media) {
      return media.element ? media.asset : media;
    };

    var getMedia = function (media) {
      return media.element ? media : ctrl.filter(function (m) {
        return m.asset === media;
      }).pop();
    };

    // controller

    var ctrl = {
      slot: slot,

      media: [],
      current: [slot.assets[0]],

      state: {
        selection: null,
      },

      registerMedia: function (media) {
        ctrl.media.push(media);

        media.$scope.$on('$destroy', function () {
          ctrl.deregisterMedia(media);
        });
      },

      deregisterMedia: function (media) {
        var i = ctrl.media.indexOf(media);

        if (i > -1) {
          ctrl.media.splice(i, 1);
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
        return ['video', 'image'].indexOf(page.types.assetMimeArray(asset, true)[0]) > -1;
      },

      hasTime: function (media) {
        var asset = getAsset(media);
        return ['video'].indexOf(page.types.assetMimeArray(asset, true)[0]) > -1;
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

    ctrl.clock = new page.slotClock(slot, ctrl);

    // callbacks

    var callbackPlay = function () {
      ctrl.media.forEach(function (m) {
        if (ctrl.hasTime(m) && ctrl.hasDuration(m)) {
          if (ctrl.isNowPlayable(m)) {
            m.element.play();
          } else if (!ctrl.isPaused(m)) {
            m.element.pause();
          }
        }
      });
    };

    var callbackJump = function () {
      ctrl.media.forEach(function (m) {
        if (ctrl.hasTime(m) && ctrl.hasDuration(m) && ctrl.isNowPlayable(m)) {
          m.element.currentTime = (ctrl.clock.position - m.asset.segment[0]) / 1000;
        }
      });
    };

    var callbackPause = function () {
      ctrl.media.forEach(function (media) {
        if (ctrl.hasTime(media) && ctrl.hasDuration(media)) {
          media.element.pause();
        }
      });
    };

    ctrl.clock.playFn(callbackPlay);
    ctrl.clock.jumpFn(callbackJump);
    ctrl.clock.pauseFn(callbackPause);

    // return

    $scope.ctrl = ctrl;
    return ctrl;
  }
]);
