'use strict';

module.directive('spreadsheet', [
  'pageService', function (page) {
    var controller = [
      '$scope', function ($scope) {

	var volume = $scope.volume;

	var getSlot = function (slot) {
	  if ('records' in slot)
	    return slot;
	  angular.extend(slot, volume.sessions[slot.id]);
	  if (!angular.isUndefined(slot.segment)) {
	    slot.records = slot.records.filter(function (rec) {
	      return page.types.segmentOverlaps(slot.segment, rec.segment);
	    });
	  }
	  return slot;
	};

	var slots = [];
	angular.forEach(volume.sessions, function (s) {
	  slots.push(s);
	});
	// slots = slots.map(getSlot);

	var data = {
	  id: [],
	  segment: [],
	  date: [],
	  top: [],
	  consent: [],
	};

	var processSlot = function (slot, i) {
	  data.id[i] = slot.id;
	  data.segment[i] = slot.segment;
	  data.date[i] = slot.date;
	  data.top[i] = slot.top;
	  data.consent[i] = slot.consent;
	  var counts = {};
	  angular.forEach(slot.records, function (rec) {
	    var record = volume.records[rec.id];
	    var c;
	    if (!(record.category in counts)) {
	      if (!(record.category in data))
		data[record.category] = [[]];
	      counts[record.category] = 1;
	      c = 0;
	    } else {
	      c = counts[record.category] ++;
	      if (!(c in data[record.category]))
		data[record.category][c] = [];
	    }
	    data[record.category][c][i] = record;
	  });
	};

	var process = function () {
	  angular.forEach(slots, processSlot);
	};

	process();
	$scope.data = data;
      }
    ];

    return {
      restrict: 'E',
      scope: {
	volume: '=',
      },
      templateUrl: 'spreadsheet.html',
      controller: controller,
    };
  }
]);
