'use strict';

module.directive('slotAnnotations', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs',
      function ($scope, $element, $attrs) {
	var ctrl = page.$parse($attrs.ctrl)($scope);
	var notes = this;
	notes.list = [];

	var $list, $head;

	notes.noteStyle = function (note) {
	  var left, right;

	  if (angular.isArray(note.record.segment)) {
	    left = note.record.segment && note.record.segment[0] ? (note.record.segment[0] - ctrl.clock.start) / (ctrl.clock.duration - ctrl.clock.start) : 0;
	    right = note.record.segment && note.record.segment[1] ? (ctrl.clock.duration - note.record.segment[1]) / (ctrl.clock.duration - ctrl.clock.start) : 0;
	  } else if (angular.isNumber(note.record.segment)) {
	    left = (note.record.segment - ctrl.clock.start) / (ctrl.clock.duration - ctrl.clock.start);
	    right = 1 - left;
	  }

	  return {
	    'z-index': note.level,
	    'top': 24 * (note.level + 1),
	    'left': (angular.isNumber(left) ? left * 100 : 0) + '%',
	    'right': (angular.isNumber(right) ? right * 100 : 0) + '%',
	  };
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
	      if (!ctrl.records[mode]) {
		ctrl.noteMode = null;
		$head.height(0);
		return;
	      }

	      ctrl.mode = page.constants.data.category[mode].name;

	      angular.forEach(ctrl.records[mode], function (record) {
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
	  $scope.$watch('ctrl.noteMode', drawNotes);
	  drawNotes(ctrl.noteMode);
	});

	return notes;
      }
    ];

    //

    return {
      restrict: 'E',
      scope: false,
      templateUrl: 'slotAnnotations.html',
      controller: controller,
      controllerAs: 'notes',
    };
  }
]);
