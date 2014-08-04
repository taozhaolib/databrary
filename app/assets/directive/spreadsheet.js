'use strict';

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXREC = 100; // maximum number of records per category per slot
    var byNumber = function(a,b) { return a-b; };
    var byString = function(a,b) { return a>b?1:a<b?-1:0; };
    var byType = function(a,b) {
      var ta = typeof a;
      var tb = typeof b;
      if (ta > tb) return 1;
      if (ta < tb) return -1;
      if (ta == 'number') return a-b;
      return a>b?1:a<b?-1:0;
    };

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
	  $scope.recordCols = Object.keys(records).sort(byNumber).map(function (c) {
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

	var sort = function (values, reverse, compare) {
	  if (!compare)
	    compare = byType;
	  var rev = reverse ? -1 : 1;
	  var i = Object.keys(meta.id).sort(function (i, j) {
	    return rev*compare(values[i], values[j]);
	  });
	  angular.forEach(meta, function (l, f) {
	    meta[f] = i.map(function (i) {
	      return l[i];
	    });
	  });
	  angular.forEach(records, function (r) {
	    angular.forEach(r, function (l, m) {
	      r[m] = i.map(function (i) {
		return l[i];
	      });
	    });
	  });
	};

	var currentSort;

	var sortBy = function (name, values) {
	  var rev = currentSort === name;
	  sort(values, rev);
	  currentSort = name + (rev ? ':rev' : '');
	};

	$scope.sortByMeta = function (f) {
	  sortBy('meta:' + f, meta[f]);
	};

	$scope.sortByMetric = function (rc, m) {
	  sortBy('metric:' + rc + ':' + m, records[rc][m]);
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
