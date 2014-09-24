'use strict';

module.directive('slotAnnotations', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element',
      function ($scope, $element) {
	var notes = this;
	notes.list = [];

	var $list, $head;

	notes.noteStyle = function (note) {
	  var style = $scope.positionStyle(note.record.segment);
	  style['z-index'] = note.level;
	  style.top = 24 * (note.level + 1);
	  return style;
	};

	var drawNote = function (levels, level, record) {
	  levels[level].push(record);
	  notes.list.push({
	    record: record,
	    level: level,
	  });
	};

	var drawNotes = function (mode) {
	  notes.list = [];
	  var levels = [];

	  switch (mode) {
	    case 'comments':
	      // todo: comments
	      $head.height(24);
	      break;

	    default:
	      if (!$scope.records[mode]) {
		$scope.noteMode = null;
		$head.height(0);
		return;
	      }

	      angular.forEach($scope.records[mode], function (record) {
		var l = levels.length;

		if (!record.segment) {
		  levels.push([]);
		  drawNote(levels, l, record);
		  return;
		}

		// look for the first level that has enough space
		for (var i = 0; i < l; i++) { // try existing levels
		  var m = levels[i].length;

		  // skip the loop if this level has a full-width record
		  if (m === 1 && !levels[i][0].segment) {
		    continue;
		  }

		  // if this record conflicts with any of this level, break
		  for (var j = 0; j < m; j++) {
		    if ((record.segment[0] && levels[i][j].segment[1] && levels[i][j].segment[1] >= record.segment[0]) || (record.segment[1] && levels[i][j].segment[0] && levels[i][j].segment[0] <= record.segment[1])) {
		      break;
		    }
		  }

		  // all good? then push
		  drawNote(levels, i, record);
		  return;
		}

		// no current level works? add a new one.
		levels.push([]);
		drawNote(levels, i, record);
	      });

	      $head.height(24 * (levels.length + 1));
	  }
	};

	page.$timeout(function () {
	  $list = $element.find('.slot-note-list');
	  $head = $element.find('.slot-note-head');
	  $scope.$watch('noteMode', drawNotes);
	  drawNotes($scope.noteMode);
	});

	return notes;
      }
    ];

    //

    return {
      restrict: 'E',
      scope: false,
      templateUrl: 'slot/annotations.html',
      controller: controller,
      controllerAs: 'notes',
    };
  }
]);
