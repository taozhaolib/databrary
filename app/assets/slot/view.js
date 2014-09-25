'use strict';
/* jslint eqnull:true */

module.controller('slot/view', [
  '$scope', 'slot', 'edit', 'pageService', 'Store', 'Segment',
  function ($scope, slot, editing, page, Store, Segment) {
    page.display.title = slot.displayName;
    $scope.flowOptions = Store.flowOptions;
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
	click: function(){
	    var track = $scope.current;
	    var baseUrl = editing ? slot.route : slot.editRoute();
	    page.$location.url(baseUrl).search('asset', track && track.asset && track.asset.id);
	}
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
	video[0].currentTime = (o - $scope.current.asset.segment.base) / 1000;
      $scope.position = o;
    }

    $scope.seekPosition = function (pos) {
      var o = positionOffset(pos);
      if (o)
	seekOffset(o);
    };

    $scope.play = function () {
      if (video)
	video[0].play();
      else
        $scope.playing = 1;
    };

    $scope.pause = function () {
      if (video)
	video[0].pause();
      else
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

    function Track(asset) {
      Store.call(this, slot, asset);
    }
    Track.prototype = Object.create(Store.prototype);
    Track.prototype.constructor = Track;

    Track.prototype.setAsset = function (asset) {
      Store.prototype.setAsset.call(this, asset);
      updateRange(asset.segment);
      if (targetAsset == asset.id)
	this.select();
    };

    function selectTrack(track) {
      if ($scope.current && $scope.form.edit)
	$scope.current.dirty = $scope.form.edit.$dirty;

      $scope.current = track;
      page.$location.search('asset', track && track.asset && track.asset.id);
      targetAsset = undefined;
      if ($scope.current) {
	$scope.playing = 0;
	if ($scope.current.asset && isFinite($scope.current.asset.segment.l))
	  $scope.position = $scope.current.asset.segment.l;
	if($scope.form.edit){
	  if ($scope.current.dirty)
	    $scope.form.edit.$setDirty();
	  else
	    $scope.form.edit.$setPristine();
	}
	delete $scope.replace;
      }
    }

    Track.prototype.select = function (event) {
      if ($scope.current !== this)
	return selectTrack(this);
      if (event)
	$scope.seekPosition(event.clientX);
    };

    Track.prototype.positionStyle = function () {
      return this.asset && $scope.positionStyle(this.asset.segment);
    };

    if (editing) {
      var removed = function(track) {
        if (track.asset || track.file)
          return;
        if (track === $scope.current)
          selectTrack(undefined);
        $scope.tracks.remove(track);
      };

      Track.prototype.remove = function () {
        var r = Store.prototype.remove.call(this);
        if (!(r && r.then))
          removed(this);
        else {
          var track = this;
          r.then(function (done) {
            if (done)
              removed(track);
          });
        }
      };

      Track.prototype.save = function () {
        var track = this;
        Store.prototype.save.call(this).then(function (done) {
          if (!done)
            return;
          delete track.dirty;
          if (track === $scope.current)
            $scope.form.edit.$setPristine();
          sortTracks();
        });
      };

      Track.prototype.upload = function (file) {
        var track = this;
	delete $scope.replace;
        Store.prototype.upload.call(this, file).then(function (done) {
          if (!done)
            return;
          // add a new blank track
	  if (!track.asset)
	    $scope.tracks.push(new Track());
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

    $scope.fileSuccess = Store.fileSuccess;
    $scope.fileProgress = Store.fileProgress;

    var videoEvents = {
      pause: function () {
	$scope.playing = 0;
      },
      playing: function () {
	$scope.playing = 1;
      },
      ratechange: function () {
	$scope.playing = video[0].playbackRate;
      },
      timeupdate: function () {
	if ($scope.current && $scope.current.asset)
	  $scope.position = $scope.current.asset.segment.base + 1000*video[0].currentTime;
      },
      ended: function () {
	$scope.playing = 0;
	/* look for something else to play? */
      },
    };
    for (var ve in videoEvents)
      videoEvents[ve] = $scope.$lift(videoEvents[ve]);

    this.deregisterVideo = function (v) {
      if (video !== v)
	return;
      video = undefined;
      v.off(videoEvents);
    };

    this.registerVideo = function (v) {
      if (video)
	this.deregisterVideo(video);
      video = v;
      v.on(videoEvents);
    };

    function sortRecords() {
      slot.records.forEach(function (r) {
        if (!r.record)
          r.record = slot.volume.records[r.id];
      });
      slot.records.sort(function (a, b) {
        return a.record.category - b.record.category || a.id - b.id;
      });
      var t = [];
      slot.records.forEach(function (r) {
        var s = r.segment = Segment.make(r.segment);
        updateRange(s);
        var overlaps = function(r) {
          return s.overlaps(r.segment);
        };
        for (var i = 0; i < t.length; i ++)
          if (!t.some(overlaps))
            break;
      });
    }

    $scope.current = undefined;

    $scope.range = new page.models.Segment(Infinity, -Infinity);
    updateRange(page.models.Segment.full); // implicitly initialize from slot.segment

    $scope.tracks = slot.assets.map(function (asset) {
      return new Track(asset);
    });
    if (editing)
      $scope.tracks.push(new Track());
    sortTracks();

    sortRecords();

    $scope.playing = 0;
    $scope.position = undefined;
    

    
    /////// OLD

    var sortRecordsOld = function () {
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

    sortRecordsOld();
  }
]);
