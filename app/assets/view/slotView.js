'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', function ($scope, slot, page) {
    page.display.title = page.types.slotName(slot);
    page.display.toolbarLinks = [];

    // helpers

    var getAsset = function (media) {
      return media && media.id ? media.asset : media;
    };

    var getMedia = function (media) {
      return media && media.id ? media : ctrl.filter(function (m) {
        return m.asset === media;
      }).pop();
    };

    // controller

    var ctrl = {
      slot: slot,
      segment: page.types.segmentParse(page.$routeParams.segment),

      media: [],

      // TODO: current can be used for multiple assets with minor mods
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

      isCurrent: function (media) {
        if (media.asset && media.asset.id)
          return ctrl.current[0] === media;
        else
          return ctrl.current[0] === media.asset;
      },

      select: function (media) {
        ctrl.current[0] = media.asset;
      },

      jump: function (asset) {
        var $track = $('#slot-timeline-track-' + asset.asset.id);
        page.display.scrollTo($track);
      },

      hasPosition: function (media) {
        var asset = getAsset(media);
        return !angular.isNothing(asset.segment);
      },

      hasDuration: function (media) {
        var asset = getAsset(media);
        return Array.isArray(asset.segment);
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

      isReady: function (media) {
        return $('#' + media.id)[0].readyState >= 4;
      },

      isPaused: function (media) {
        return $('#' + media.id)[0].paused;
      },
    };

    // clock

    ctrl.clock = new page.slotClock(slot, ctrl);

    // sort records

    var sortRecords = function () {
      ctrl.records = {};
      ctrl.noteOptions = {
        'comments': 'comments',
      };

      angular.forEach(slot.records, function (record) {
        if (!(record.category in ctrl.records)) {
          ctrl.records[record.category] = [];
          ctrl.noteOptions[record.category] = page.constants.data.category[record.category].name;
        }

        ctrl.records[record.category].push(record);
      });
    };

    sortRecords();

    // callbacks

//    var callbackPlay = function () {
//      ctrl.media.forEach(function (m) {
//        if (ctrl.hasTime(m) && ctrl.hasDuration(m)) {
//          if (ctrl.isNowPlayable(m)) {
//            m.element.play();
//          } else if (!ctrl.isPaused(m)) {
//            m.element.pause();
//          }
//        }
//      });
//    };
//
//    var callbackJump = function () {
//      ctrl.media.forEach(function (m) {
//        if (ctrl.hasTime(m) && ctrl.hasDuration(m) && ctrl.isNowPlayable(m)) {
//          m.element.currentTime = (ctrl.clock.position - m.asset.segment[0]) / 1000;
//        }
//      });
//    };
//
//    var callbackPause = function () {
//      ctrl.media.forEach(function (media) {
//        if (ctrl.hasTime(media) && ctrl.hasDuration(media)) {
//          media.element.pause();
//        }
//      });
//    };

    var callbackTime = function () {
      var isTimed = ctrl.hasDuration(ctrl.current[0]);
      var switched = false;

      ctrl.media.forEach(function (media) {
        if (!ctrl.hasDisplay(media) || !ctrl.hasTime(media)) {
          return;
        }

        var el = $('#' + media.id).find('video')[0];

        if (ctrl.clock.playing) {
          if (ctrl.isNowPlayable(media) && el.paused) {
            el.play();
          } else if (!el.paused) {
            el.pause();
          }
        } else {
          if (!el.paused) {
            el.pause();
          }
        }
      });
    };

//    ctrl.clock.playFn(callbackPlay);
//    ctrl.clock.jumpFn(callbackJump);
//    ctrl.clock.pauseFn(callbackPause);
    ctrl.clock.timeFn(callbackTime);

    // failsafe

    $scope.$on('$destroy', function () {
      ctrl.clock.pause();
    });

    // return

    $scope.ctrl = ctrl;
    return ctrl;
  }
]);
