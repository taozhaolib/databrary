'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', function ($scope, slot, page) {
    var volume = slot.volume;
    page.display.title = slot.displayName;

    // helpers

    var getAsset = function (media) {
      return media && media.id ? media.asset : media;
    };

    var getElement = function (media) {
      return $('#' + media.id).find('video')[0];
    };

    // controller

    var ctrl = {
      slot: slot,
      volume: volume,
      segment: slot.segment,

      media: [],

      // TODO: current can be used for multiple assets with minor mods
      current: [slot.assets[0]],

      state: {
	selection: null,
      },

      syncPlayback: function (media) {
        var el = getElement(media);

	if (ctrl.clock.playing && el && el.paused) {
	  ctrl.clock.play();
	} else if (el && !el.paused) {
	  ctrl.clock.pause();
	}
      },

      registerMedia: function (media) {
	ctrl.media.push(media);
	ctrl.syncPlayback(media);

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
	if (media.asset && media.asset.id) {
	  return ctrl.current[0] === media;
	}
	else {
	  return ctrl.current[0] === media.asset;
	}
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
	return asset && !angular.isNothing(asset.segment);
      },

      hasDuration: function (media) {
	var asset = getAsset(media);
	return asset && Array.isArray(asset.segment);
      },

      hasDisplay: function (media) {
	var asset = getAsset(media);
	if (!asset)
	  return asset;
	var type = asset.asset.format.type;
	return type === 'video' || type === 'image';
      },

      hasTime: function (media) {
	var asset = getAsset(media);
	return asset && asset.asset.format.type === 'video';
      },

      isNowPlayable: function (media) {
	var asset = getAsset(media);
	return ctrl.clock.position > asset.segment[0] && ctrl.clock.position < asset.segment[1];
      },

      isReady: function (media) {
	return getElement(media).readyState >= 4;
      },

      isPaused: function (media) {
	return getElement(media).paused;
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

      angular.forEach(volume.containers[slot.container.id].records, function (record) {
	if (!(volume.records[record.id].category in ctrl.records)) {
	  ctrl.records[volume.records[record.id].category] = [];
	  ctrl.noteOptions[volume.records[record.id].category] = page.constants.category[volume.records[record.id].category].name;
	}

	ctrl.records[volume.records[record.id].category].push(record);
      });
    };

    sortRecords();

    // callbacks

    var asapMediaFn = function (fn) {
      ctrl.media.forEach(function (media) {
	if (ctrl.hasTime(media) && ctrl.hasDisplay(media)) {
	  var el = getElement(media);
	  var $el = $(el);

	  var cb = function () {
	    if (el.readyState === 4) {
	      fn(media, el);
	    } else {
	      $el.one('loadeddata', function () {
		cb();
	      });
	    }
	  };

	  cb();
	}
      });
    };

    var callbackPlay = function () {
      asapMediaFn(function (media, el) {
	if (ctrl.isNowPlayable(media) && ctrl.isPaused(media)) {
	  el.currentTime = (ctrl.clock.position - media.asset.segment[0]) / 1000;
	  el.play();
	} else if (!ctrl.isPaused(media)) {
	  el.pause();
	}
      });
    };

    var callbackJump = function () {
      asapMediaFn(function (media) {
	ctrl.syncPlayback(media);
      });
    };

    var callbackPause = function () {
      asapMediaFn(function (media, el) {
	el.pause();
      });
    };

    var callbackTime = function () {
      asapMediaFn(function (media, el) {
	if (ctrl.isNowPlayable(media) && el.paused && !el.seeking) {
	  ctrl.syncPlayback(media);
	}
      });
    };

    ctrl.clock.playFn(callbackPlay);
    ctrl.clock.jumpFn(callbackJump);
    ctrl.clock.pauseFn(callbackPause);
    ctrl.clock.timeFn(callbackTime);

    // failsafe

    $scope.$on('$destroy', function () {
      ctrl.clock.pause();
    });

    // return

    $scope.$watch('page.$location.search().mode', function (val) {
      if (val === 'edit') {
	ctrl.mode = 'edit';
	page.display.toolbarLinks = [
	  {
	    type: 'yellow',
	    html: page.constants.message('slot.view'),
	    click: function () {
	      page.$location.search('mode', 'view');
	    },
	    access: page.permission.VIEW,
	    object: volume,
	  },
	];
      } else {
	ctrl.mode = 'view';
	page.display.toolbarLinks = [
	  {
	    type: 'yellow',
	    html: page.constants.message('slot.edit'),
	    click: function () {
	      page.$location.search('mode', 'edit');
	    },
	    access: page.permission.CONTRIBUTE,
	    object: volume,
	  },
	];
      }
    });

    $scope.ctrl = ctrl;
    return ctrl;
  }
]);
