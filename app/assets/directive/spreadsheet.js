'use strict';

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXREC = 100; // maximum number of records per category per slot
    var byNumber = function(a,b) { return a-b; };
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

	// all arrays indexed over row (column-major)
	var meta = {
	  id: [],
	  segment: [],
	  date: [],
	  top: [],
	  consent: [],
	};
	var records = {}; // [category + count/MAXREC][metric] = []
	var depends = {}; // [recordid] = rows

	var populateSlot = function (slot, i) {
	  // populate meta:
	  meta.id[i] = slot.id;
	  meta.segment[i] = slot.segment;
	  meta.date[i] = slot.date;
	  meta.top[i] = slot.top;
	  meta.consent[i] = slot.consent;

	  var counts = {};
	  angular.forEach(slot.records, function (rec) {
	    var record = volume.records[rec.id];

	    // populate records:
	    if (!(record.category in counts))
	      counts[record.category] = 0;
	    else if (counts[record.category] >= MAXREC) {
	      page.$log.warn("Spreadsheet overflow for record category " + record.category + " on slot " + slot.id);
	      return;
	    }
	    var c = record.category + (counts[record.category]++)/MAXREC;
	    var r;
	    if (!(c in records)) {
	      r = records[c] = {/*id: []*/};
	      if (record.category === page.category.participant.id)
		r.age = [];
	    } else
	      r = records[c];

	    // populate measures:
	    /*r.id[i] = rec.id;*/
	    if ('age' in r)
	      r.age[i] = rec.age;
	    angular.forEach(record.measures, function (v, m) {
	      if (!(m in r))
		r[m] = [];
	      r[m][i] = v;
	    });

	    // populate depends:
	    if (!(record.id in depends))
	      depends[record.id] = [i];
	    else
	      depends[record.id].push(i);
	  });
	};

	var populateCols = function () {
	  meta.records = Object.keys(records).sort(byNumber).map(function (c) {
	    var cat = page.constants.data.category[Math.floor(c)];
	    return {
	      id: c,
	      category: cat,
	      metrics: Object.keys(records[c]).sort(byNumber)
	    };
	  });
	};

	var populate = function () {
	  angular.forEach(slots, populateSlot);
	  populateCols();
	};

	populate();
	$scope.meta = meta;
	$scope.records = records;
      }
    ];

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'spreadsheet.html',
      controller: controller,
    };
  }
]);
