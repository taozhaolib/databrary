'use strict';
/* jslint eqnull:true */

module.controller('slotView', [
  '$scope', 'slot', 'edit', 'pageService',
  function ($scope, slot, editing, page) {
    page.display.title = slot.displayName;
    $scope.slot = slot;
    $scope.volume = slot.volume;
    $scope.editing = editing;
    $scope.mode = editing ? 'edit' : 'view';
    $scope.form = {};

    var targetAsset = page.$location.search().asset;
    var video;

    if (editing || slot.checkPermission(page.permission.EDIT))
      page.display.toolbarLinks.push({
	type: 'yellow',
	html: page.constants.message(editing ? 'slot.view' : 'slot.edit'),
	url: editing ? slot.route : slot.editRoute(),
      });
    

    function updateRange(segment) {
      if (isFinite(slot.segment.l))
	$scope.range.l = slot.segment.l;
      else if (isFinite(segment.l) && segment.l < $scope.range.l)
	$scope.range.l = segment.l;
      
      if (isFinite(slot.segment.u))
	$scope.range.u = slot.segment.u;
      else if (isFinite(segment.u) && segment.u > $scope.range.u)
	$scope.range.u = segment.u;
    }

    function offsetPosition(offset) {
      return (offset - $scope.range.base) / ($scope.range.u - $scope.range.l);
    }

    function positionOffset(position) {
      var tl = $(document.getElementById('slot-timeline'));
      var p = (position - tl.offset().left) / tl.outerWidth();
      if (p < 0 || p > 1)
	return;
      return $scope.range.l + p * ($scope.range.u - $scope.range.l);
    }

    $scope.positionStyle = function (p) {
      var styles = {}, l, r;
      if (p == null)
	return styles;
      if (p instanceof page.models.Segment) {
	l = offsetPosition(p.l);
	r = offsetPosition(p.u);
      } else
	l = offsetPosition(p);
      if (l >= 0 && l <= 1)
	styles.left = 100*l + '%';
      if (r >= 0 && r <= 1)
	styles.right = 100 - 100*r + '%';
      return styles;
    };

    function seekOffset(o) {
      if (video && $scope.current && $scope.current.asset && $scope.current.asset.segment.contains(o))
	video.currentTime = (o - $scope.current.asset.segment.base) / 1000;
      $scope.position = o;
    }

    $scope.seekPosition = function (pos) {
      var o = positionOffset(pos);
      if (o)
	seekOffset(o);
    };

    $scope.play = function () {
      if (video)
	video.play();
      $scope.playing = 1;
    };

    $scope.pause = function () {
      if (video)
	video.pause();
      $scope.playing = 0;
    };

    function sortTracks() {
      if (!$scope.tracks)
	return;
      $scope.tracks.sort(function (a, b) {
	return !a.asset - !b.asset || !a.file - !b.file ||
	  (a.asset ?
	    isFinite(b.asset.segment.l) - isFinite(a.asset.segment.l) ||
	    a.asset.segment.l - b.asset.segment.l ||
	    a.asset.segment.u - b.asset.segment.u ||
	    a.id - b.id :
	    b.progress - a.progress);
      });
    }

    /* There is a lot of unfortunate duplication between here and volumeEditMaterialsForm,
     * but this should be considered authoritative. */

    function Track(asset) {
      if (asset)
	this.setAsset(asset);
      else
	this.fillData();
    }

    Track.prototype.setAsset = function (asset) {
      this.asset = asset;
      updateRange(asset.segment);
      if (targetAsset == asset.id)
	this.select();
      if (editing)
	this.fillData();
    };

    function selectTrack(track) {
      if ($scope.current && $scope.form.edit)
	$scope.current.dirty = $scope.form.edit.$dirty;
      $scope.current = track;
      page.$location.search('asset', track && track.asset && track.asset.id);
      targetAsset = undefined;
      if ($scope.current && $scope.form.edit) {
	if ($scope.current.dirty)
	  $scope.form.edit.$setDirty();
	else
	  $scope.form.edit.$setPristine();
      }
      delete $scope.replace;

      $scope.playing = 0;
      if ($scope.current.asset && isFinite($scope.current.asset.segment.l))
	$scope.position = $scope.current.asset.segment.l;
    }

    Track.prototype.select = function (event) {
      if ($scope.current !== this)
	return selectTrack(this);
      if (event)
	$scope.seekPosition(event.clientX);
    };

    Object.defineProperty(Track.prototype, 'name', {
      get: function () {
	if (!(this.file || this.asset))
	  return page.constants.message('asset.add');
	return this.file && this.file.file.name || this.asset && this.asset.name || page.constants.message('file');
      }
    });

    Track.prototype.positionStyle = function () {
      return this.asset && $scope.positionStyle(this.asset.segment);
    };

    if (editing) {
      Track.prototype.fillData = function () {
	var asset = this.asset || {
	  classification: page.classification.RESTRICTED
	};
	this.data = {
	  name: asset.name,
	  classification: asset.classification+''
	};
      };

      Track.prototype.remove = function () {
	var track = this;
	if (!confirm(page.constants.message('asset.remove.confirm')))
	  return;
	track.asset.remove().then(function() {
	  page.messages.add({
	    type: 'green',
	    countdown: 3000,
	    body: page.constants.message('asset.remove.success', track.name),
	  });
	  if (track === $scope.current)
	    selectTrack();
	  $scope.tracks.remove(track);
	}, function (res) {
	  page.messages.addError({
	    type: 'red',
	    body: page.constants.message('asset.remove.error', track.name),
	    report: res,
	  });
	});
      };

      Track.prototype.save = function () {
	var track = this;
	var act;
	if (track.file) {
	  track.data.upload = track.file.uniqueIdentifier;
	  act = track.asset ? track.asset.replace(track.data) : slot.createAsset(track.data);
	} else
	  act = track.asset.save(track.data);

	act.then(function (asset) {
	  if (asset instanceof page.models.Asset) {
	    track.asset.asset = asset;
	    asset = track.asset;
	  }
	  track.setAsset(asset);

	  page.messages.add({
	    type: 'green',
	    countdown: 3000,
	    body: page.constants.message('asset.' + (track.file ? 'upload' : 'update') + '.success', track.name) +
	      (track.file && asset.format.transcodable ? ' ' + page.constants.message('asset.upload.transcoding') : ''), 
	  });

	  if (track.file) {
	    if (!('creation' in track.asset.asset))
	      track.asset.asset.creation = {date: Date.now(), name: track.file.file.name}; 
	    track.file.cancel();
	    delete track.file;
	    delete track.progress;
	  }
	  delete track.dirty;
	  if (track === $scope.current)
	    $scope.form.edit.$setPristine();
	  sortTracks();
	}, function (error) {
	  page.messages.addError({
	    type: 'red',
	    body: page.constants.message('asset.update.error', track.name),
	    report: error,
	  });
	  if (track.file) {
	    track.file.cancel();
	    delete track.file;
	    delete track.progress;
	    delete track.data.upload;
	  }
	});
      };

      Track.prototype.upload = function (file) {
	var track = this;
	track.file = file;
	track.progress = 0;
	file.track = track;
	delete $scope.replace;

	page.assets.assetStart(file).then(function () {
	  file.resume();
	  if (!track.asset)
	    $scope.tracks.push(new Track());
	},
	function (error) {
	  page.messages.addError({
	    type: 'red',
	    body: page.constants.message('asset.upload.rejected', track.name), 
	    report: error,
	  });
	  file.cancel();
	  delete track.file;
	  delete track.progress;
	});
      };

      Track.prototype.replace = function () {
	$scope.replace = !$scope.replace;
      };
    }

    $scope.fileAdded = function (file) {
      if (!editing)
	return;

      if ($scope.current)
	$scope.current.upload(file);
    };

    $scope.fileSuccess = function (file) {
      file.track.progress = 100;
      file.track.save();
    };

    $scope.fileProgress = function (file) {
      file.track.progress = file.progress();
    };

    var videoEvents = {
      pause: function () {
	$scope.playing = 0;
      },
      playing: function () {
	$scope.playing = 1;
      },
      ratechange: function (event) {
	console.log(event);
      },
      timeupdate: function (event) {
	console.log(event);
      },
      ended: function () {
	$scope.playing = 0;
	/* look for something else to play? */
      },
    };

    this.deregisterVideo = function (v) {
      if (video !== v)
	return;
      console.log("no video");
      video = undefined;
      v.off(videoEvents);
    };

    this.registerVideo = function (v) {
      if (video)
	this.deregisterVideo(video);
      video = v;
      console.log(video);
      v.on(videoEvents);
    };

    $scope.range = new page.models.Segment(Infinity, -Infinity);
    updateRange(page.models.Segment.full);

    $scope.current = undefined;
    $scope.tracks = slot.assets.map(function (asset) {
      return new Track(asset);
    });
    if (editing)
      $scope.tracks.push(new Track());
    sortTracks();

    $scope.playing = 0;
    $scope.position = $scope.leftTime;
    



    /////// OLD

    var sortRecords = function () {
      $scope.records = {};
      $scope.noteOptions = {
	comments: 'comments',
      };

      angular.forEach(slot.records, function (record) {
	if (!(slot.volume.records[record.id].category in $scope.records)) {
	  $scope.records[slot.volume.records[record.id].category] = [];
	  $scope.noteOptions[slot.volume.records[record.id].category] = page.constants.category[slot.volume.records[record.id].category].name;
	}

	$scope.records[slot.volume.records[record.id].category].push(record);
      });
    };

    sortRecords();
  }
]);
