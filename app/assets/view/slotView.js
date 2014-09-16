'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'pageService', 'assetService', function ($scope, slot, page, assets) {
    var volume = slot.volume;
    page.display.title = slot.displayName;
    $scope.flowOptions = assets.flowOptions;
    
    // helpers

    function getAsset(media) {
      return media && '$scope' in media ? media.asset : media;
    }

    function getElement(media) {
      return $('#' + media.id).find('video')[0];
    }

    // upload
    $scope.fileAdded = function(file, e) {
      var tl = $scope.ctrl.timeline;
      assets.assetStart(file).then(function(){
	file.pause();
	tl.uploadsInProgress.push(angular.copy(file)); //create a better object here. let uploadsInProgress have editable metadata
	file.resume();
      });
    };

    $scope.fileSuccess = function(file) {
	var tl = $scope.ctrl.timeline;
	var data = {
	    name: file.file.name,
	    classification: 0,
	    upload: file.uniqueIdentifier
	};
	ctrl.slot.createAsset(data).then(function(res){
	    for(var i in tl.uploadsInProgress){
	      if (tl.uploadsInProgress[i].uniqueIdentifier === file.uniqueIdentifier){
		tl.uploadsInProgress.splice(i, 1);	  
	      }
	    }
	    tl.tracks.push(res);
	});
    };

    // controller

    function syncPlayback(media) {
      var el = getElement(media);

      if (ctrl.clock.playing && el && el.paused) {
	ctrl.clock.play();
      } else if (el && !el.paused) {
	ctrl.clock.pause();
      }
    }

    var ctrl = {
      slot: slot,
      volume: volume,
      segment: slot.segment,

      media: [],

      current: slot.assets[0],

      registerMedia: function (media) {
	ctrl.media.push(media);
	syncPlayback(media);

	media.$scope.$on('$destroy', function () {
	  ctrl.deregisterMedia(media);
	});
      },

      deregisterMedia: function (media) {
	ctrl.media.remove(media);
      },

      setCurrent: function (asset) {
	ctrl.current = getAsset(asset);
	if(ctrl.current && ctrl.current.asset) ctrl.updateEditData();
      },

      isCurrent: function (media) {
	return ctrl.current === getAsset(media);
      },

      jump: function (asset) {
	var $track = $('#slot-timeline-track-' + asset.asset.id);
	page.display.scrollTo($track);
      },

      hasPosition: function (media) {
	var asset = getAsset(media);
	return asset && isFinite(asset.segment.l);
      },

      hasDisplay: function (media) {
	var asset = getAsset(media);
	if (!asset)
	  return false;
	var type = asset.asset.format.type;
	return type === 'video' || type === 'image';
      },

      hasTime: function (media) {
	var asset = getAsset(media);
	return asset && asset.asset.duration;
      },

      isNowPlayable: function (media) {
	var asset = getAsset(media);
	return ctrl.clock.position > asset.segment.l && ctrl.clock.position < asset.segment.u;
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
	comments: 'comments',
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
	  el.currentTime = (ctrl.clock.position - media.asset.segment.l) / 1000;
	  el.play();
	} else if (!ctrl.isPaused(media)) {
	  el.pause();
	}
      });
    };

    var callbackJump = function () {
      asapMediaFn(syncPlayback);
    };

    var callbackPause = function () {
      asapMediaFn(function (media, el) {
	el.pause();
      });
    };

    var callbackTime = function () {
      asapMediaFn(function (media, el) {
	if (ctrl.isNowPlayable(media) && el.paused && !el.seeking) {
	  syncPlayback(media);
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
