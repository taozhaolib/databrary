'use strict';

module.controller('slotView', [
  '$scope', 'slot', 'edit', 'pageService',
  function ($scope, slot, editing, page) {
    $scope.slot = slot;
    var volume = $scope.volume = slot.volume;
    $scope.editing = editing;
    $scope.mode = editing ? 'edit' : 'view';
    page.display.title = slot.displayName;
    
    if (editing || page.models.Login.checkAccess(page.permission.EDIT, slot))
      page.display.toolbarLinks.push({
	type: 'yellow',
	html: page.constants.message(editing ? 'slot.view' : 'slot.edit'),
	url: editing ? slot.route : slot.editRoute(),
      });

    $scope.tracks = [];
    $scope.uploadsInProgress = [];

    // helpers

    // TODO - update for addition of formData
    function getAsset(media) {
      return media && '$scope' in media ? media.asset : media;
    }

    function getElement(media) {
      return $('#' + media.id).find('video')[0];
    }

    var slotAssetToTrack = function(x){
      return {
	slotAsset:  x,
	formData:   {
	  id: x.asset.id,
	  name: x.asset.name,
	  classification: x.asset.classification+'',
	}
      };
    };

    var flowFileToUploadInProgress = function(x){
      var ans = {
	flowFile: x,
	formData: {
	  name: x.replace ? ctrl.current.name : undefined,
	  classification: (x.replace ? ctrl.current.classification : 0) + ''
	},
      };
      x.backRef = ans;
      return ans;
    };

    $scope.parseTracks = function () {
      $scope.tracks = $.map(ctrl.slot.assets, slotAssetToTrack); //this is only valid in the beginning, before it wiped out
      ctrl.timeline.time.left = ctrl.clock.start;
      ctrl.timeline.time.right = ctrl.clock.duration;

      $scope.tracks.sort(function sortTracksFn(a, b) {
	var al = a.slotAsset.segment.l;
	var bl = b.slotAsset.segment.l;
	return isFinite(bl) - isFinite(al) ||
	  al - bl ||
	  a.slotAsset.segment.u - b.slotAsset.segment.u ||
	  a.id - b.id;
      });
      if (!ctrl.current) ctrl.setCurrent($scope.tracks[0]); //might be a better place for this...
    };

    // upload
    $scope.fileAdded = function(file) {
      if ($scope.ctrl.current && $scope.ctrl.replaceable){
	file.replace = $scope.ctrl.current.asset.id;
      }

      page.assets.assetStart(file).then(function(){
	file.pause();
	var newTrack = flowFileToUploadInProgress(file);
	$scope.uploadsInProgress.push(newTrack); 
	file.resume();
	ctrl.setCurrent(newTrack);
      },
      function(error){
	page.messages.addError({
	  type: 'red',
	  body: page.constants.message('asset.upload.rejected', file.file.name), 
	  report: error,
	});
      }
      );
    };

    $scope.fileSuccess = function(file) {
	var data = file.backRef.formData;
	data.upload = file.uniqueIdentifier;

	if(!file.replace){
	  ctrl.slot.createAsset(data).then(function(res){
	      removeUploadInProgress(file.backRef);
	      var newTrack = slotAssetToTrack(res);
	      $scope.tracks.push(newTrack);
	      ctrl.setCurrent(newTrack); //conditional on isCurrent?
	      page.messages.add({
		type: 'green',
		countdown: 3000,
		body: page.constants.message('asset.upload.success', res.name) + (res.asset.format.transcodable ? page.constants.message('asset.upload.transcoding') : ''), 
	      });
	  },
	  function(error){
	      removeUploadInProgress(file.backRef);
	      page.messages.addError({
		type: 'red',
		body: page.constants.message('asset.update.error', data.name),
		report: error
	      });
	  });
	}
	else{
	  ctrl.current.replace(file.data).then(function(res){
	    ctrl.replaceable = false;
	    removeUploadInProgress(file.backRef);
	    var newTrack = slotAssetToTrack(res);

	    //perform replacement in tracks list
	    for(var i in $scope.tracks){
	      if($scope.tracks[i].slotAsset.asset.id == file.replace){
		$scope.tracks[i] = slotAssetToTrack(newTrack);
		break;
	      }
	    }
	    ctrl.setCurrent(newTrack); //condition on isCurrent?
	    page.messages.add({
	      type: 'green',
	      countdown: 3000,
	      body: page.constants.message('asset.upload.success', res.asset.name) + (res.asset.format.transcodable ? page.constants.message('asset.upload.transcoding') : ''), 
	    });
	  });
	}
    };

    $scope.saveAsset = function() {
      ctrl.current.slotAsset.save(ctrl.current.formData).then(function (res){
	  ctrl.assetEditForm.$setPristine();
	  ctrl.assetEditForm.messages.add({
	    type: 'green',
	    countdown: 3000,
	    body: page.constants.message('asset.update.success', res.name),
	  });
	},
	function (error){
	  ctrl.assetEditForm.messages.addError({
	    type: 'red',
	    body: page.constants.message('asset.update.error', ctrl.current.formData.name),
	    report: error,
	});
      }
      );
    };

    $scope.removeAsset = function(conf){
      if(conf && !confirm(page.constants.message('asset.remove.confirm'))) return;
      ctrl.current.slotAsset.remove().then(function() {
	page.messages.add({
	  type: 'green',
	  countdown: 3000,
	  body: page.constants.message('asset.remove.success', ctrl.current.slotAsset.asset.name || page.constants.message('file')),
	});
	$scope.tracks.remove(ctrl.current); 
	$scope.ctrl.current = undefined;
      }, function (res) {
	page.messages.addError({
	  type: 'red',
	  body: page.constants.message('asset.remove.error', ctrl.current.slotAsset.asset.name || page.constants.message('file')),
	  report: res,
	});
      });
    };

    var removeUploadInProgress = function (file){
	for(var i in $scope.uploadsInProgress){
	  if ($scope.uploadsInProgress[i].uniqueIdentifier === file.uniqueIdentifier){
	    $scope.uploadsInProgress.splice(i, 1);	  
	  }
	}
    };

    $scope.fileProgress = function(file){
	file.progressVal = file.progress();
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

    //TODO - update 'has' functions to reflect addition of formData
    var ctrl = {
      slot: slot,
      volume: volume,
      segment: slot.segment,
      mode: $scope.mode,

      media: [],

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

      setCurrent: function (track) {
	if(ctrl.current && ctrl.assetEditForm){
	  ctrl.current.formData.dirty = ctrl.assetEditForm.$dirty;
	}
	ctrl.current = track;
	ctrl.replaceable = false;
	if(track && ctrl.assetEditForm){
	  if(track.formData.dirty){
	    ctrl.assetEditForm.$setDirty();
	  } else {
	    ctrl.assetEditForm.$setPristine();
	  }
	}
      },

      isCurrent: function (media) {
	return ctrl.current === getAsset(media);
      },

      toggleReplace: function() {
	ctrl.replaceable = !ctrl.replaceable;
      },

      jump: function (asset) {
	var $track = $('#slot-timeline-track-' + asset.asset.id);
	page.display.scrollTo($track);
      },

      hasPosition: function (media) {
	var asset = getAsset(media);
	return asset && asset.segment && isFinite(asset.segment.l);
      },

      hasDisplay: function (media) {
	var asset = getAsset(media);
	if (!asset || !asset.slotAsset)
	  return false;
	asset = asset.slotAsset;
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

    $scope.ctrl = ctrl;
    return ctrl;
  }
]);
