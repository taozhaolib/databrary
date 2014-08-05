'use strict';

/* Remember, angular.forEach is 8-10x slower: http://jsperf.com/angularloops */

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXREC = 64; // maximum number of records per category per slot
    var MAXLEN = 32; // maximum number of records per category per slot
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
	  slots.push(/*getSlot*/(s));
	});

	var count = slots.length;
	var order = Object.keys(slots);

	// all arrays indexed over row (column-major)
	var meta = {
	  id: [],
	  segment: [],
	  date: [],
	  top: [],
	  consent: [],
	};
	var records = {}; // [category + count/MAXREC][metric] = []
	var recordCols = [], metricCols = [];
	var depends = {}; // [recordid] = rows

	var populateSlot = function (i) {
	  var slot = slots[i];
	  
	  // populate meta:
	  meta.id[i] = slot.id;
	  meta.segment[i] = slot.segment;
	  meta.date[i] = slot.date;
	  meta.top[i] = slot.top;
	  meta.consent[i] = slot.consent;

	  var counts = {};
	  for (var ri = 0; ri < slot.records.length; ri ++) {
	    var record = volume.records[slot.records[ri].id];

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
	      r = records[c] = {id: []};
	      if (record.category === page.category.participant.id)
		r.age = [];
	    } else
	      r = records[c];

	    // populate measures:
	    r.id[i] = record.id;
	    if ('age' in r)
	      r.age[i] = slot.records[ri].age;
	    for (var m in record.measures) {
	      if (!(m in r))
		r[m] = [];
	      r[m][i] = record.measures[m];
	    }

	    // populate depends:
	    if (!(record.id in depends))
	      depends[record.id] = [i];
	    else
	      depends[record.id].push(i);
	  }
	};

	var populateCols = function () {
	  metricCols = [];
	  recordCols = Object.keys(records).sort(byNumber).map(function (c) {
	    var cat = page.constants.data.category[Math.floor(c)];
	    var metrics = Object.keys(records[c]).filter(function (m) {
	      return m !== 'id' && !(m in page.constants.data.metric && page.constants.data.metric[m].long);
	    });
	    if (metrics.length)
	      metrics.sort(byNumber);
	    else
	      metrics = ['id'];
	    metricCols.push.apply(metricCols, metrics.map(function (metric) {
	      return {
		record: c,
		metric: metric
	      };
	    }));
	    return {
	      id: c,
	      category: cat,
	      metrics: metrics
	    };
	  });
	};

	var populate = function () {
	  for (var i = 0; i < count; i ++)
	    populateSlot(i);
	  populateCols();
	};

	var rows = [];

	var generateCell = function (r, c, i) {
	  var td = r.appendChild(document.createElement('td'));
	  if (!angular.isUndefined(c)) {
	    if (typeof c === 'string' && c.length >= MAXLEN)
	      c = c.substr(0, MAXLEN) + '...';
	    td.appendChild(document.createTextNode(c));
	  }
	  td.id = i;
	  return td;
	};
	
	var generateRow = function (i) {
	  var row = rows[i] = document.createElement('tr');
	  row.id = 'ss-row:' + i;
	  if (meta.top[i])
	    row.classList.add('ss-top');
	  generateCell(row, meta.date[i], 'ss-date:' + i);
	  var consentName = page.constants.data.consent[meta.consent[i]];
	  var consent = generateCell(row, consentName, 'ss-consent:' + i);
	  if (consentName) {
	    consent.classList.add(consentName.toLowerCase());
	    consent.setAttribute('hint', 'consent-' + consentName);
	  }
	  var ri, mi;
	  for (ri = 0; ri < recordCols.length; ri ++) {
	    var record = recordCols[ri];
	    var r = record.id;
	    var rec = records[r];
	    var metrics = record.metrics;
	    for (mi = 0; mi < metrics.length; mi ++) {
	      var m = metrics[mi];
	      var v = rec[m][i];
	      if (m === 'age')
		v = page.display.formatAge(v);
	      generateCell(row, v, 'ss-rec:' + r + ':' + m + ':' + i);
	    }
	  }
	};

	var regenerateAges = function () {
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var m = metricCols[mi];
	    if (m.metric !== 'age')
	      continue;
	    for (var i = 0; i < count; i ++) {
	      var v = records[m.record][m.metric][i];
	      if (!angular.isUndefined(v))
		document.getElementById('ss-rec:' + m.record + ':age:' + i).firstChild.replaceWholeText(page.display.formatAge(v));
	    }
	  }
	};

	var generate = function () {
	  for (var i = 0; i < count; i ++)
	    generateRow(i);
	};

	var fill = function () {
	  var ss = document.getElementById('ss');

	  for (var i = 0; i < count; i ++)
	    ss.appendChild(rows[order[i]]);
	};

	var sort = function (values, compare) {
	  if (!compare)
	    compare = byType;
	  order.sort(function (i, j) {
	    return compare(values[i], values[j]);
	  });
	};

	var currentSort;

	var sortBy = function (name, values) {
	  if (currentSort === name)
	    order.reverse();
	  else {
	    sort(values);
	    currentSort = name;
	  }
	  fill();
	};

	$scope.sortByMeta = function (f) {
	  sortBy('meta:' + f, meta[f]);
	};

	$scope.sortByMetric = function (rc, m) {
	  sortBy('metric:' + rc + ':' + m, records[rc][m]);
	};

	populate();
	generate();
	fill();
	$scope.recordCols = recordCols;
	$scope.metricCols = metricCols;

	page.events.listen($scope, 'displayService-toggleAge', regenerateAges);
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
